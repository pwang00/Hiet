{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Control.Monad
import qualified Control.Monad as Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import PietTypes (Vector)
import PietTypes
import Interpreter
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)
import Data.Vector ((!), (!?))
import Debug.Trace

cmdTable :: Vector PietInstr
cmdTable = Vec.fromList $ [Nop, Add, Div, Grt, Dup, CharIn, 
                      Push, Sub, Mod, Ptr, Roll, IntOut, 
                      Pop, Mul, Not, Swi, IntIn, CharOut]

unaryStackOps = [Pop, Push, Not, Dup]
arithmeticOps = [Add, Sub, Mul, Div, Mod]
pointerOps = [Ptr, Swi]
inOutOps = [IntIn, IntOut, CharIn, CharOut]
initialResult = (Res initialState Continue)

binAOpFromPietOp :: (Integral a) => PietInstr -> (a -> a -> a)
binAOpFromPietOp instr 
  | instr == Add = (+)
  | instr == Sub = (-)
  | instr == Mul = (*)
  | instr == Div = (div)
  | instr == Mod = (mod)

hues :: [Hue]
hues = [Red, Yellow, Green, Cyan, Blue, Magenta] 

lightness :: [Hue -> Lightness]
lightness = [Light, Reg, Dark]

colors :: Vector Lightness
colors = Vec.fromList $ [ color hue | hue <- hues, color <- lightness ] ++ [White, Black]

instance Arbitrary Stack where
  arbitrary :: Gen Stack
  arbitrary = sized gen where
      gen :: Int -> Gen Stack
      gen 0 = frequency
          [(1, return $ Stack [])
          ,(1, Stack . return <$> choose (1, 1000000)) -- Represent integers that might appear after ops
          ,(2, Stack . return <$> choose (1, 256)) -- All ASCII
          ,(2, Stack . return <$> choose (32, 127))] -- Printable character range

      gen n = do
          (Stack x) <- gen (n `div` 2)
          (Stack y) <- gen (n `div` 3)
          (Stack z) <- gen (n `div` 4)
          return $ Stack (x ++ y ++ z)

instance Arbitrary Lightness where
  arbitrary :: Gen Lightness
  arbitrary = do
    idx <- choose (0, length colors - 1)
    return $ colors ! idx
  
  shrink :: Lightness -> [Lightness]
  shrink l = []

instance Arbitrary PietProgram where
  arbitrary :: Gen PietProgram
  arbitrary = do
    width <- choose (1, 15)
    height <- choose (1, 15)
    let bound = vectorOf height (vectorOf width (arbitrary :: Gen Lightness))
    grid <- liftM (Vec.fromList . liftM Vec.fromList) bound
    
    return $ Prog {
      _width = width, -- Int
      _height = height, -- Int
      _cs = 1, -- Int
      _grid = grid -- Vector (Vector Lightness)
    }

  -- No logical shrink for a fixed size program
  shrink :: PietProgram -> [PietProgram]
  shrink prog = []


instance Arbitrary ProgramState where
  arbitrary :: Gen ProgramState
  arbitrary = do
      stack <- (arbitrary :: Gen Stack)
      dp <- (arbitrary :: Gen DirectionPtr)
      cc <- (arbitrary :: Gen CodelChooser)
      sen <- (arbitrary :: Gen Int)
      cb <- choose (0, 1) :: Gen Int
      row <- choose (0, 14)
      col <- choose (0, 14)
      rctr <- choose (0, 8)
      let inbuf = if sen > 0 then [sen] else [] :: [Int] -- the in and out buffers can never both be filled
      let outbuf = if sen < 0 then [-sen] else [] :: [Int] 

      return State {_stack = stack,
                    _dp = dp,
                    _cc = cc,
                    _cb = cb,
                    _rctr = rctr,
                    _pos = (row, col),
                    _inbuf = inbuf,
                    _outbuf = outbuf}


instance Arbitrary PietInstr where
  arbitrary :: Gen PietInstr
  arbitrary = do
      n <- choose (0, (Vec.length cmdTable) - 1)
      return $ cmdTable ! n

  shrink :: PietInstr -> [PietInstr]
  shrink cmd = []

instance Arbitrary DirectionPtr where
  arbitrary :: Gen DirectionPtr
  arbitrary = oneof
          [return (DP DPRight)
          ,return (DP DPLeft)
          ,return (DP DPDown)
          ,return (DP DPUp)]

  shrink :: DirectionPtr -> [DirectionPtr]
  shrink dp = []

instance Arbitrary CodelChooser where
  arbitrary :: Gen CodelChooser
  arbitrary = oneof
    [return (CC CCLeft)
    ,return (CC CCRight)]
  
  shrink :: CodelChooser -> [CodelChooser]
  shrink dir = []

-- Tests to make sure we do arithmetic correctly
prop_RotateDP :: DirectionPtr -> Int -> Bool
prop_RotateDP dp@(DP dir) n = rotate dp n == rotate dp (n `mod` 4)

prop_SwitchCC :: CodelChooser -> Int -> Bool 
prop_SwitchCC cc@(CC dir) n = switch cc n == switch cc (n `mod` 2)


-- Checking that binary arithmetic stack instructions decrease the stack size by 1
-- And correctly flush the 
prop_BinaryStackInstrs :: ProgramState -> PietInstr -> Property
prop_BinaryStackInstrs state@(State{_stack = Stack stk, _inbuf = ib}) instr = 
  elem instr arithmeticOps && (length stk) > 1 ==> 
    let (Res state' action) = execInstr state instr
        stack@(Stack stk') = _stack state' in 
    (length stk') == (length stk + length ib - 1) && _inbuf state' == []

-- Testing binary arithmetic operations
prop_binAOps :: PietInstr -> ProgramState -> Property
prop_binAOps instr state@(State{_stack = Stack stk, _inbuf = ib}) =
  elem instr arithmeticOps && (length stk) > 1 && ib == [] ==>
    let (Res state' action) = execInstr state instr
        (x:y:xs) = stk
        stack@(Stack (x':xs')) = _stack state'
        op = binAOpFromPietOp instr in
    (y `op` x) == x'

prop_Not :: ProgramState -> Property
prop_Not state@(State{_stack = Stack stk, _inbuf = ib}) = 
  (length stk) > 0 && ib == [] ==>
      let (Res state' action) = execInstr state Not
          (x:xs) = stk
          stack@(Stack (x':xs')) = _stack state' in
      (x == 0) == (x' /= 0)

prop_Dup :: ProgramState -> Property
prop_Dup state@(State{_stack = Stack stk, _inbuf = ib}) = 
  (length stk) > 0 && ib == [] ==>
      let (Res state' action) = execInstr state Dup
          (x:xs) = stk
          stack@(Stack (x':y:xs')) = _stack state' in
      x == y && x == x'

-- Testing that push increases the stack size by 1, and that the top of the stack is 
-- the value of cb
prop_Push :: ProgramState -> Property
prop_Push state@(State{_stack = Stack stk, _inbuf = ib, _cb = cb}) = 
  ib == [] ==> 
    let (Res state' action) = execInstr state Push
        stack@(Stack stk'@(x:xs)) = _stack state' in
  x == cb && (length stk') == (length stk + 1)

-- Testing that pop decreases stack length by 1
prop_Pop :: ProgramState -> Property
prop_Pop state@(State{_stack = Stack stk, _inbuf = ib, _cb = cb}) = 
  ib == [] ==> 
    let (Res state' action) = execInstr state Pop
        stack@(Stack stk') = _stack state' in
  (length stk) == 0 && (length stk' == 0) || (length stk' == length stk - 1)

-- Testing that CharOut pops from the stack and moves it to our out buffer
prop_Output :: ProgramState -> Property
prop_Output state@(State{_stack = Stack stk, _inbuf = ib, _outbuf = ob, _cb = cb}) =
  (length stk) > 0 && ib == [] ==> 
    let (Res state' action) = execInstr state CharOut
        (x:xs) = stk
        stack@(Stack stk') = _stack state' 
        ob' = _outbuf state' in
    
  (length stk') == (length stk - 1) && ob' == [x]

-- Checks that the interpreter never tries to search outside of the program grid
prop_CheckPositionInBounds :: PietProgram -> Int -> Bool
prop_CheckPositionInBounds prog input = 
  let transitions = stepN prog initialResult 30 input in
    all (\(Res st _) -> checkBoundaries prog (_pos st)) transitions

-- Checks that a program terminates after the retries counter reaches 8 or 
-- if we reach max transitions (currently 30)
prop_CheckTermination :: PietProgram -> Int -> Bool
prop_CheckTermination prog input = 
  let transitions = stepN prog initialResult 30 input in
    all (\(Res st _) -> _rctr st <= 8) transitions || (length transitions) == 30

-- An invariant of every Piet Program under this interpreter design is that the input and output buffers should 
-- never exceed length 1, since every time a value is pushed into either of these it is either
-- flushed from the input buffer and pushed onto the stack, or flushed from the output buffer
-- into stdout.
prop_CheckInOutBufs :: PietProgram -> Int -> Bool
prop_CheckInOutBufs prog input = 
  let transitions = stepN prog initialResult 30 input in
    all (\(Res st _) -> length (_inbuf st) <= 1 && length (_outbuf st) <= 1) transitions

--Generates a sequence of states during program execution
--Similar to interp but we discard IO since we can simulate it 
--By having quickCheck generate a random number and pushing it to the stack
stepN :: PietProgram -> PietResult -> Int -> Int -> [PietResult]
stepN prog (Res state action) 0 _ = []
stepN prog (Res state EndProg) _ _ = []
stepN prog res@(Res state action) n input 
  | action == Continue = (step prog state) : stepN prog res (n - 1) input
  | action == IntInRequest = (step prog state{_inbuf = [n]}) : stepN prog res (n - 1) input
  | action == CharInRequest = (step prog state{_inbuf = [n]}) : stepN prog res (n - 1) input
  | action == IntOutRequest = (step prog state{_outbuf = []}) : stepN prog res (n - 1) input
  | action == CharOutRequest = (step prog state{_outbuf = []}) : stepN prog res (n - 1) input

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  _ <- runTests
  return ()