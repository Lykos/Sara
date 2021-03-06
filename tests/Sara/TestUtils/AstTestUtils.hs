module Sara.TestUtils.AstTestUtils where

import Control.Monad.Except
import Text.Parsec.Pos
import qualified Sara.Ast.Syntax as S
import qualified Sara.Errors as E
import Sara.Parser.PrettyPrinter
import Sara.Ast.Meta
import Sara.Ast.AstUtils
import Test.QuickCheck.Property

clearPositions :: ParserProgram -> ParserProgram
clearPositions = mapNodeMetas $ const $ mkNodeMeta

clearTypes :: TypeCheckerProgram -> ParserProgram
clearTypes = mapExpressionMetas $ const ()

clearPureness :: PureCheckerProgram -> SymbolizerProgram
clearPureness = mapExpressionMetas $ \(ExpressionMeta t _) -> TypMeta t

clearSymbols :: SymbolizerProgram -> TypeCheckerProgram
clearSymbols = mapVariableMetas (const ()) . mapFunctionMetas (const ())

mkNodeMeta :: NodeMeta
mkNodeMeta = NodeMeta position

testfile :: String
testfile = "<testfile>"

-- | Standard source position that is used for all tests.
position :: SourcePos
position = newPos testfile 0 0

check :: (Eq a', Eq b', Eq c', Eq d') => S.Program a b c d -> E.ErrorOr (S.Program a' b' c' d') -> E.ErrorOr (S.Program a' b' c' d') -> Property
check input expected actual = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ render expected
                  ++ "\n\nActual:\n" ++ render actual
                  ++ "\n\nInput:\n" ++ prettyRender input

render :: E.ErrorOr (S.Program a b c d) -> String
render e = case runExcept e of
  Left e  -> show e
  Right r -> prettyRender r
