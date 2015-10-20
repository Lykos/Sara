{-# LANGUAGE TemplateHaskell #-}

module RegressionTest (regressionCheck) where

import Errors
import PrettyPrinter
import AstTestUtils
import Compiler
import Syntax
import TestUtils
import RegressionTestUtils

import System.FilePath
import System.Directory
import Data.Int
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Test.QuickCheck.Monadic as M
import Test.QuickCheck.Property

isExpected :: Either Error Int64 -> Int64 -> Bool
isExpected (Left _)  _ = False
isExpected (Right n) m = n == m

checkRight :: String -> String -> IO (Bool, String)
checkRight fname input = do
  let expected = parseExpectation fname input
  actual <- runExceptT $ run nopReporter fname input
  let example = "\nFile: " ++ fname
                ++ "\n\nExpected:\n" ++ show expected
                ++ "\n\nInput:\n" ++ input
                ++ "\n\nActual:\n" ++ show actual
  return (isExpected actual expected, example)

prop_regressionsWork :: Property
prop_regressionsWork = once $ M.monadicIO $ do
  inputs <- M.run getInputs
  results <- M.run $ mapM (uncurry checkRight) inputs
  let failedResults = filter (not . fst) results
  let result = case failedResults of
        []     -> (True, "")
        (x:xs) -> x
  M.monitor $ counterexample $ snd result
  M.assert $ fst result

extension :: String
extension = ".sara"

testDir :: String
testDir = "test_files"

getInputs :: IO [(String, String)]
getInputs = do
  dir <- getCurrentDirectory
  let testDir' = dir </> testDir
  files <- getDirectoryContents testDir'
  let saraFiles = filter ((extension ==) . takeExtension) files
  let readFile' f = readFile $ testDir' </> f
  inputs <- mapM readFile' saraFiles
  return $ saraFiles `zip` inputs

return []

regressionCheck = $quickCheckAll
