module Sara.RegressionTest (regressionGroup) where

import Sara.Errors
import Sara.PrettyPrinter
import Sara.AstTestUtils
import Sara.Compiler
import Sara.Syntax
import Sara.TestUtils
import Sara.RegressionTestUtils

import System.FilePath
import System.Directory
import Data.Int
import Control.Monad.Except
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as M
import Test.QuickCheck.Property

isExpected :: Either Error Int64 -> Expectation -> Bool
isExpected (Left e) (Errors f)   = e == f
isExpected (Right n) (Returns m) = n == m
isExpected _ _                   = False

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
testDir = "tests/test_files"

getInputs :: IO [(String, String)]
getInputs = do
  dir <- getCurrentDirectory
  let testDir' = dir </> testDir
  files <- getDirectoryContents testDir'
  let saraFiles = filter ((extension ==) . takeExtension) files
  let readFile' f = readFile $ testDir' </> f
  inputs <- mapM readFile' saraFiles
  return $ saraFiles `zip` inputs

regressionGroup = testGroup "Regression Tests" [ testProperty "regression tests work" prop_regressionsWork ]
