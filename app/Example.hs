module Example where
  
import Control.Monad
import Control.Monad.Trans.State
import Interpreter
import PietTypes
import ImageLoader
import Codec.Picture
import Data.Vector ((!), (!?))

path :: String
path = "/home/philip2000/Documents/CMSC488B/final-project/piet-interpreter/images/adder1.png"

example :: IO ()
example = do
  res <- imageToProgram path 1
  case res of
    (Left err) -> return ()
    (Right img) -> do
      finalState <- evalStateT (interp img) (Res initialState Continue)
      return ()