module AstTestUtils where

clearPositions :: ParserProgram -> ParserProgram
clearPositions = mapNodeMetas $ const $ mkNodeMeta

clearTypes :: TypeCheckerProgram -> ParserProgram
clearTypes = mapExpressionMetas $ const ()

clearSymbols :: SymbolizerProgram -> TypeCheckerProgram
clearSymbols = mapVariableMetas (const ()) . mapFunctionMetas (const ())
