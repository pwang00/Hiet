module Interpreter where

import Control.Lens
import PietTypes
import ImageLoader
import Codec.Picture
import Data.Vector ((!), (!?))
import Data.Char (ord, chr)
import Data.Function (on)
import Data.List
import Data.Monoid
import Data.Foldable
import System.IO

-- Rotates direction pointer 
rotate :: DirectionPtr -> Int -> DirectionPtr
rotate (DP x) y = DP $ toEnum ((fromEnum x + y) `mod` 4)

switch :: CodelChooser -> Int -> CodelChooser
switch (CC x) y = CC $ toEnum ((fromEnum x + y) `mod` 2)

checkBoundaries :: PietProgram -> Position -> Bool
checkBoundaries prog (row, col) = 
  (row >= 0 && row < _height prog && col >= 0 && col < _width prog)

-- Finds all codels in a color block
-- Maybe I'll use an O(1) queue or something
computeBlock :: PietProgram -> Position -> Lightness -> [Position]
computeBlock prog@(Prog {_grid = grid, _cs = cs}) pos color = 
  nub $ floodfill [pos] [pos] where
    floodfill :: [Position] -> [Position] -> [Position]
    floodfill [] block = block 
    floodfill (top@(r, c) : stack') block = floodfill (adj ++ stack') (adj ++ block) 
        where 
          valid = \lc@(y, x) -> (checkBoundaries prog lc && grid ! y ! x == color 
                                  && notElem lc block)
          up = (r - cs, c)
          down = (r + cs, c)
          left = (r, c - cs)
          right = (r, c + cs)
          adj = filter valid [up, down, left, right]

codelFromPositions :: [Position] -> DirectionPtr -> CodelChooser -> Position
codelFromPositions nextBlockEntry dp cc =
  let cond = case dp of 
        (DP DPRight) -> case cc of 
          (CC CCLeft) -> maxCol <> minRow
          (CC CCRight) -> maxCol <> maxRow
        (DP DPDown) -> case cc of 
          (CC CCLeft) -> maxRow <> minCol
          (CC CCRight) -> maxRow <> maxCol
        (DP DPLeft) -> case cc of
          (CC CCLeft) -> minCol <> maxRow
          (CC CCRight) -> minCol <> minRow
        (DP DPUp) -> case cc of
          (CC CCLeft) -> minRow <> minCol
          (CC CCRight) -> minRow <> maxCol
        where 
          minRow = (flip compare `on` fst)
          minCol = (flip compare `on` snd)
          maxRow = (compare `on` fst)
          maxCol = (compare `on` snd) in
  maximumBy cond nextBlockEntry

moveInDir :: PietProgram -> Position -> DirectionPtr -> Position
moveInDir prog@(Prog {_cs = cs}) pos@(r, c) dp = 
  case dp of 
    (DP DPRight) -> (r, c + cs)
    (DP DPDown) -> (r + cs, c)
    (DP DPLeft) -> (r, c - cs)
    (DP DPUp) -> (r - cs, c)

execInstr :: ProgramState -> PietInstr -> PietResult
execInstr state@(State {_inbuf = ib, _outbuf = ob}) instr = 
  let dp = _dp state
      cc = _cc state
      cb = _cb state
      flushed = flushIB ib state -- Flushes the input buffer onto the stack
      state' = case instr of 
        Add -> op2 stk (+) flushed
        Sub -> op2 stk (-) flushed
        Mul -> op2 stk (*) flushed
        Div -> op2 stk (div) flushed
        Mod -> op2 stk (mod) flushed
        Not -> op1 stk (fromEnum . (== 0)) flushed
        Grt -> op2 stk ((fromEnum .) . (>)) flushed 
        Dup -> dup stk flushed
        Roll -> roll stk flushed
        Swi -> chptr stk 0 dp cc flushed
        Ptr -> chptr stk 1 dp cc flushed
        Push -> flushed {_stack = Stack (cb : s)}
        Pop -> pop stk flushed
        IntIn -> flushed
        CharIn -> flushed
        IntOut -> flushOB s flushed
        CharOut -> flushOB s flushed
        Nop -> flushed
    
      ib' = _inbuf state'
      ob' = _outbuf state'
      action = case instr of 
        CharIn -> if (length ib') == 0 then CharInRequest else Continue
        CharOut -> if (length ob') == 1 then CharOutRequest else Continue
        IntIn -> if (length ib') == 0 then IntInRequest else Continue
        IntOut -> if (length ob') == 1 then IntOutRequest else Continue
        _ -> Continue 

      updatedState = state'{_inbuf = []} in
      
      
      Res updatedState action

    where
      
      -- We need to flush the io buffer onto the stack to ensure that integers are pushed in the correct order
      -- before performing operations

      stk@(Stack s) = _stack (flushIB ib state)
      flushIB :: [Int] -> ProgramState -> ProgramState
      flushIB inBuf st@(State {_stack = (Stack s)}) = st{_stack = Stack (inBuf ++ s), _inbuf = []}

      flushOB :: [Int] -> ProgramState -> ProgramState
      flushOB s' st = if (length s') >= 1 then 
                        let (x:xs) = s' in
                            st{_stack = Stack xs, _outbuf = [x]} else st
      
      -- Unary arithmetic stack ops
      op1 :: Stack -> (Int -> Int) -> ProgramState -> ProgramState
      op1 = \s op st -> case s of 
                          Stack (x:xs) -> state {_stack = Stack $ (op x) : xs}
                          _ -> st 
      -- Binary arithmetic stack ops
      op2 :: Stack -> (Int -> Int -> Int) -> ProgramState -> ProgramState
      op2 = \s op st -> case s of 
                          Stack (x:y:xs) -> st {_stack = Stack $ (y `op` x):xs}
                          _ -> st

      pop :: Stack -> ProgramState -> ProgramState
      pop = \s st -> case s of 
                       Stack (x:xs) -> st {_stack = Stack xs}
                       _ -> st

      chptr :: Stack -> Int -> DirectionPtr -> CodelChooser -> ProgramState -> ProgramState
      chptr = \s t dp cc st -> case (s, t) of 
                          (Stack (x:xs), 0) -> st {_stack = Stack xs, _cc = switch cc x}
                          (Stack (x:xs), 1) -> st {_stack = Stack xs, _dp = rotate dp x}
                          _ -> st

      dup :: Stack -> ProgramState -> ProgramState
      dup = \s st -> case s of 
                        Stack (x:xs) -> state {_stack = Stack (x:x:xs)}
                        _ -> state
 
      roll :: Stack -> ProgramState -> ProgramState
      roll = \s st -> case s of
                      (Stack (x:y:xs)) -> 
                        let (rolls, depth) = (x, y)
                            (topY, rest) = (take depth xs, drop depth xs) in
                            if (depth < 0) then st 
                            else st {_stack = Stack $ (rot x topY) ++ rest}
                      _ -> st

      rot :: Int -> [a] -> [a]
      rot n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs

-- Cases for when out of bounds or encountering a black block
-- Order to update dp / cc goes: update dp -> rctr + 1 -> update cc, etc
recalculateEntry :: ProgramState -> [Position] -> ProgramState
recalculateEntry state@(State {_dp = dp, _cc = cc, _rctr = rctr}) block =
    let dp' = rotate dp 1
        cc' = switch cc 1
        rctr' = _rctr state + 1 in

        case odd rctr of 
            True ->
              let fixedPos = codelFromPositions block dp' cc in
              state{_pos = fixedPos, _rctr = rctr', _dp = dp'}
            False ->
              let fixedPos = codelFromPositions block dp cc' in
              state{_pos = fixedPos, _rctr = rctr', _cc = cc'}


-- Terminates the program if 8 attempts are made to exit the color block without success
step :: PietProgram -> ProgramState -> PietResult
step prog state@(State {_rctr = 8}) = (Res state EndProg)
step prog@(Prog {_grid = grid, _cs = cs}) state@(State {_rctr = rctr, 
                                                  _pos = pos@(r, c), _dp = dp, _cc = cc}) =
    let currCodel = grid ! r ! c
        block = computeBlock prog pos currCodel
        furthestCodelInBlock = codelFromPositions block dp cc
        nextBlockEntry@(r2, c2) = moveInDir prog furthestCodelInBlock dp
        colorAtPos = if checkBoundaries prog nextBlockEntry 
                     then Just (grid ! r2 ! c2) 
                     else Nothing in

    case colorAtPos of
        Nothing -> Res (recalculateEntry state block) Continue
        Just Black -> Res (recalculateEntry state block) Continue
        (Just nextCodel) -> let instr = (decodeInstr currCodel nextCodel)
                                (Res newState res) = execInstr state {_cb = length block} instr in 
                                Res newState {_pos = nextBlockEntry, _rctr = 0} res


-- We do IO by pushing to an input buffer and reading from an output buffer
-- when receiving an IO action (CharInRequest .. IntOutRequest).  Execution
-- otherwise proceeds normally.
interp :: PietProgram -> PietResult -> IO (ProgramState)
interp prog (Res finalState EndProg) = return finalState
interp prog (Res state@(State {_inbuf = ib, _outbuf = ob}) action)
  | action == CharInRequest = do
      putStr "Input Char: "
      hFlush stdout
      x <- getChar
      interp prog (step prog state{_inbuf = [(ord x)]})
  | action == IntInRequest = do
      putStr "Input Int: "
      hFlush stdout
      x <- getLine
      interp prog (step prog state{_inbuf = [(read x) :: Int]})
  | action == CharOutRequest = case ob of 
      [x] -> do
        putChar $ chr x
        interp prog (step prog state{_outbuf = []})
      _ -> interp prog (step prog state)
  | action == IntOutRequest = case ob of 
      [x] -> do
        putStr $ show x
        interp prog (step prog state{_outbuf = []})
      _ -> interp prog (step prog state)
  | otherwise = interp prog (step prog state)
      