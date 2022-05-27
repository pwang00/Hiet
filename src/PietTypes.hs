-- Simulates Piet's stack and direction pointer
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module PietTypes where

import Control.Monad
import qualified Data.Vector as Vec
import Codec.Picture.Types
import Control.Lens
import Control.Monad.State
import Data.Vector ((!))

type Vector = Vec.Vector

type CodelSize = Int
type Position = (Int, Int)

data DPDir = DPRight | DPDown | DPLeft | DPUp deriving (Enum, Eq, Show)
data CCDir = CCLeft | CCRight deriving (Enum, Eq, Show)

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta deriving (Enum, Eq, Show)
data Lightness = Light Hue | Reg Hue | Dark Hue | Black | White deriving (Eq, Show)

newtype Stack = Stack [Int] deriving (Show)
newtype DirectionPtr = DP { _dpdir :: DPDir } deriving (Eq, Show)
newtype CodelChooser = CC { _ccdir :: CCDir } deriving (Eq, Show)

type ImageGrid = Vector (Vector Lightness)

data ProgramState = State {
  _stack :: Stack, 
  _dp :: DirectionPtr, 
  _cc :: CodelChooser,
  _pos :: Position,
  _cb :: Int, -- Number of codels in the current color block
  _rctr :: Int, -- Retries counter: program terminates after 8 unsuccessful attempts
  _inbuf :: [Int], -- We push and pop to these depending on whether IO is done
  _outbuf :: [Int]
}

data PietProgram = Prog {
  _grid :: ImageGrid,
  _width :: Int,
  _height :: Int,
  _cs :: Int -- Codel size
} deriving (Show)

makeLenses ''PietProgram
makeLenses ''ProgramState

type PietMT = StateT PietResult IO (ProgramState)

data Action = Continue | CharInRequest | IntInRequest | CharOutRequest 
              | IntOutRequest | EndProg deriving (Eq, Show)

data PietResult = Res ProgramState Action deriving (Show)

data PietInstr = Nop | Push | Pop | Add | Sub | Mul 
                | Div | Mod | Not | Grt | Ptr | Swi
                | Dup | Roll | IntIn | IntOut | CharIn | CharOut deriving (Eq, Show)

instance Show ProgramState where
  show State {
    _stack = s,
    _dp = dp,
    _cc = cc,
    _pos = pos,
    _cb = cb,
    _rctr = rctr,
    _inbuf = ib,
    _outbuf = ob
  } = "State {\n" 
        ++ "  _stack = " ++ show s ++ ",\n"
        ++ "  _dp = " ++ show dp ++ ",\n"
        ++ "  _cc = " ++ show cc ++ ",\n"
        ++ "  _pos = " ++ show pos ++ ",\n"
        ++ "  _cb = " ++ show cb ++ ",\n"
        ++ "  _rctr = " ++ show rctr ++ ",\n"
        ++ "  _inbuf = " ++ show ib ++ ",\n"
        ++ "  _outbuf = " ++ show ob ++ "\n"
        ++ "}"

initialState = State {
  _stack = Stack [], 
  _dp = DP DPRight, 
  _cc = CC CCLeft,
  _pos = (0, 0),
  _cb = 0, -- Number of codels in the current color block
  _rctr = 0, -- Terminates the program if 8 attempts are made
  _inbuf = [],
  _outbuf = []
}