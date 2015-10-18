module Main where

import System.IO
import System.Environment
import System.Console.Haskeline

processFile :: String -> IO ()
processFile fname = withModule fname $ \m -> readFile fname >>= process m >> return ()

repl :: IO ()
repl = runInputT defaultSettings (withModule "my cool jit" $ \m -> loop m)
  where loop mod = do
          minput <- getInputLine "ready> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
              modn <- liftIO $ process mod input
              case modn of
                Just modn -> loop modn
                Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
