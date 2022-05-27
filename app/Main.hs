module Main where

import Control.Monad
import Control.Monad.Trans.State
import PietTypes
import qualified Interpreter
import ImageLoader
import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config {
  hello :: String,
  showfinal :: Bool,
  codelsize :: Int
}

config :: Parser Config
config = Config
      <$> strOption
        ( long "path" 
        <> metavar "PATH" 
        <> short 'p'
        <> help "Supply a path to Piet image, e.g. (.png, .jpg, .gif)")
      <*> switch
        ( long "Show final state"
        <> short 's'
        <> help "Whether or not to display the final program state upon termination")
      <*> option auto
        ( long "Codel size"
        <> help "Codel size to use for interpreter"
        <> short 'c'
        <> showDefault
        <> value 1
        <> metavar "INT")

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (config <**> helper)
        ( fullDesc
      <> progDesc "Interprets a Piet program image"
      <> header "Hiet: a Piet interpreter written in Haskell" )

runProgram :: Config -> IO ()
runProgram (Config path showfs cs) = do
  img <- imageToProgram path cs
  case img of 
    (Left err) -> putStrLn $ "An error was encountered: " ++ show err
    (Right img) -> do
      finalState <- evalStateT (Interpreter.interp img) (Res initialState Continue)
      case showfs of
        False -> putStrLn ""
        True -> do 
          putStrLn "=====================Final State====================="
          putStrLn $ show finalState
          return ()