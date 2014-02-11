module Main where

import LlvmParser
import Codegen
import Emit

import Control.Monad (foldM)
import "mtl" Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseTopLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

quit :: InputT IO ()
quit = outputStrLn "Until next time!" >> return ()

printHelp :: InputT IO ()
printHelp = mapM_ outputStrLn listOfHelpItems
  where
    listOfHelpItems = [":q     -- Quit"
                      ,":quit  -- Will also quit"
                      ,":h     -- This help message"
                      ,":reset -- clear the current AST"]

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "ready> "
      case minput of
        -- Do nothing
        Nothing ->  outputStrLn "Goodbye"
        
        -- We're expecting a command
        Just (':':cmd) -> case cmd of
          'h':_   -> printHelp >> loop mod
          'q':_   -> quit
          "quit"  -> quit
          "reset" -> loop initModule
          _       -> loop mod
          
        -- We've actual code input
        Just input -> do
          modn <- liftIO $ process mod input
          case modn of
            Just modn -> loop modn
            Nothing -> loop mod

runMultipleFiles :: [FilePath] -> AST.Module -> IO ()
runMultipleFiles [] _ = return ()
runMultipleFiles (f:fs) ast = do
  newast <- readFile f >>= process ast
  case newast of
    Nothing -> return ()
    Just new -> runMultipleFiles fs new

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    (fname:[]) -> processFile fname >> putStrLn "File Processed"
    ls -> runMultipleFiles ls initModule
