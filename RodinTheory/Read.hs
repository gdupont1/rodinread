module RodinTheory.Read (
        parseTheoryFile
    ) where

import Formula (Formula)
import Formula.Tokenizer (tkn)
import Data.Set (empty)
import Data.Either (fromRight)
import RodinTheory
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)
import Data.List
import Data.List.Split (splitOn)
import Internal.XML

parseNotationType :: String -> NotationType
parseNotationType "INFIX" = Infix
parseNotationType _ = Prefix

parseOperatorProp :: Element -> OperatorProp
parseOperatorProp elt =
    OperatorProp {
        associative = (lkOrDef (pref "associative") attrskv "false") == "true",
        commutative = (lkOrDef (pref "commutative") attrskv "false") == "true",
        formulaType = (lkOrDef (pref "formulaType") attrskv "false") == "true",
        notationType = parseNotationType $ lkOrDef (pref "formulaType") attrskv "PREFIX"
    }
    where attrskv = attrToTuple $ elAttribs elt

parseOperatorArgument :: Element -> OperatorArgument
parseOperatorArgument elt =
    OperatorArgument {
        expression = tkn $ lkOrDef (pref' "expression") attrskv "",
        identifier = lkOrDef (pref' "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseOperatorWDCondition :: Element -> OperatorWDCondition
parseOperatorWDCondition elt =
    OperatorWDCondition {
        predicate = tkn $ lkOrDef (pref' "predicate") (attrToTuple $ elAttribs elt) ""
    }

parseOperatorDirectDefinition :: Element -> OperatorDirectDefinition
parseOperatorDirectDefinition elt =
    OperatorDirectDefinition {
        formula = tkn $ lkOrDef (pref "formula") (attrToTuple $ elAttribs elt) ""
    }

parseRecursiveDefinitionCase :: Element -> RecursiveDefinitionCase
parseRecursiveDefinitionCase elt =
    RecursiveDefinitionCase {
        caseExpression = tkn $ lkOrDef (pref' "expression") attrskv "",
        caseFormula    = tkn $ lkOrDef (pref  "formula")    attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseOperatorRecursiveDefinition :: Element -> OperatorRecursiveDefinition
parseOperatorRecursiveDefinition elt =
    OperatorRecursiveDefinition {
        inductiveArgument = lkOrDef (pref "inductiveArgument") (attrToTuple $ elAttribs elt) "",
        cases = parseChildren (isQName' pref "recursiveDefinitionCase") parseRecursiveDefinitionCase elt
    }

parseImportTheory :: Element -> ImportTheory
parseImportTheory elt =
    ImportTheory { theory = lkOrDef (pref "importTheory") (attrToTuple $ elAttribs elt) "" }

parseImportTheoryProject :: Element -> ImportTheoryProject
parseImportTheoryProject elt =
    ImportTheoryProject {
        project = lkOrDef (pref "importTheoryProject") (attrToTuple $ elAttribs elt) "",
        theories = parseChildren (isQName' pref "importTheory") parseImportTheory elt
    }

parseTypeParameter :: Element -> TypeParameter
parseTypeParameter elt =
    TypeParameter { typeIdentifier = lkOrDef (pref' "identifier") (attrToTuple $ elAttribs elt) "" }


parseConstructorArgument :: Element -> ConstructorArgument
parseConstructorArgument elt =
    ConstructorArgument {
        caId   = lkOrDef (pref' "identifier") attrskv "",
        caType = tkn $ lkOrDef (pref  "type") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseDataTypeConstructor :: Element -> DataTypeConstructor
parseDataTypeConstructor elt =
    DataTypeConstructor {
        dtcId = lkOrDef (pref' "identifier") attrskv "",
        args  = parseChildren (isQName' pref "constructorArgument") parseConstructorArgument elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseTypeArgument :: Element -> TypeArgument
parseTypeArgument elt =
    TypeArgument {
        typeArg = tkn $ lkOrDef (pref "givenType") (attrToTuple $ elAttribs elt) ""
    }

parseDataTypeDefinition :: Element -> DataTypeDefinition
parseDataTypeDefinition elt =
    DataTypeDefinition {
        dtId = lkOrDef (pref' "identifier") (attrToTuple $ elAttribs elt) "",
        typeArguments = parseChildren (isQName' pref "typeArgument") parseTypeArgument elt,
        constructors = parseChildren (isQName' pref "datatypeConstructor") parseDataTypeConstructor elt
    }


parseNewOperatorDefinition :: Element -> NewOperatorDefinition
parseNewOperatorDefinition elt =
    NewOperatorDefinition {
        opLabel  = lkOrDef (pref' "label") attrskv "",
        opProp   = parseOperatorProp elt,
        opArgs   = parseChildren (isQName' pref "operatorArgument"            ) parseOperatorArgument            elt,
        opWD     = parseChildren (isQName' pref "operatorWDcondition"         ) parseOperatorWDCondition         elt,
        opDirDef = parseChildren (isQName' pref "directOperatorDefinition"    ) parseOperatorDirectDefinition    elt,
        opRecDef = parseChildren (isQName' pref "recursiveOperationDefinition") parseOperatorRecursiveDefinition elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseAxiomaticTypeDefinition :: Element -> AxiomaticTypeDefinition
parseAxiomaticTypeDefinition elt =
    AxiomaticTypeDefinition { aTypeId = lkOrDef (pref' "identifier") (attrToTuple $ elAttribs elt) "" }

parseAxiomaticOperatorDefinition :: Element -> AxiomaticOperatorDefinition
parseAxiomaticOperatorDefinition elt =
    AxiomaticOperatorDefinition {
        aOpProp  = parseOperatorProp elt,
        aOpLabel = lkOrDef (pref' "label") (attrToTuple $ elAttribs elt) "",
        aType    = tkn $ lkOrDef (pref "type") (attrToTuple $ elAttribs elt) "",
        aOpArgs  = parseChildren (isQName' pref "operatorArgument"   ) parseOperatorArgument    elt,
        aOpWD    = parseChildren (isQName' pref "operatorWDcondition") parseOperatorWDCondition elt
    }

parseAxiomaticDefinitionAxiom :: Element -> AxiomaticDefinitionAxiom
parseAxiomaticDefinitionAxiom elt =
    AxiomaticDefinitionAxiom {
        aDefLabel     = lkOrDef (pref' "label"    ) attrskv "",
        aDefPredicate = tkn $ lkOrDef (pref' "predicate") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseAxiomaticDefinitionsBlock :: Element -> AxiomaticDefinitionsBlock
parseAxiomaticDefinitionsBlock elt =
    AxiomaticDefinitionsBlock {
        aDefBLabel = lkOrDef (pref' "label") (attrToTuple $ elAttribs elt) "",
        aDefBTypes = parseChildren (isQName' pref "axiomaticTypeDefinition"    ) parseAxiomaticTypeDefinition     elt,
        aDefBDef   = parseChildren (isQName' pref "axiomaticOperatorDefinition") parseAxiomaticOperatorDefinition elt,
        aDefBAx    = parseChildren (isQName' pref "axiomaticDefinitionAxiom"   ) parseAxiomaticDefinitionAxiom    elt
    }

parseTheorem :: Element -> Theorem
parseTheorem elt =
    Theorem {
        thName      = lkOrDef (pref' "label"    ) attrskv "",
        thPredicate = tkn $ lkOrDef (pref' "predicate") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseMetaVariable :: Element -> MetaVariable
parseMetaVariable elt =
    MetaVariable {
        mvId   = lkOrDef (pref' "identifier") attrskv "",
        mvType = tkn $ lkOrDef (pref  "type"      ) attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseInferenceGiven :: Element -> InferenceGiven
parseInferenceGiven elt =
    InferenceGiven {
        givenPredicate = tkn $ lkOrDef (pref' "predicate") attrskv "",
        givenHyp       = (lkOrDef (pref  "hyp"      ) attrskv "") == "true"
    }
    where attrskv = attrToTuple $ elAttribs elt

parseInferenceInfer :: Element -> InferenceInfer
parseInferenceInfer elt = 
    InferenceInfer {
        inferPredicate = tkn $ lkOrDef (pref' "predicate") (attrToTuple $ elAttribs elt) ""
    }

parseInferenceRule :: Element -> InferenceRule
parseInferenceRule elt =
    InferenceRule {
        infLabel = lkOrDef (pref' "label") attrskv "",
        infApp   = lkOrDef (pref  "applicability") attrskv "",
        infDesc  = lkOrDef (pref  "desc") attrskv "",
        given    = parseChildren (isQName' pref "given") parseInferenceGiven elt,
        infer    = parseChildren (isQName' pref "infer") parseInferenceInfer elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseRewriteRuleRHS :: Element -> RewriteRuleRHS
parseRewriteRuleRHS elt =
    RewriteRuleRHS {
        rhsLabel     = lkOrDef (pref' "label") attrskv "",
        rhsPredicate = tkn $ lkOrDef (pref' "predicate") attrskv "",
        rhsFormula   = tkn $ lkOrDef (pref "formula") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseRewriteRule :: Element -> RewriteRule
parseRewriteRule elt =
    RewriteRule {
        rewLabel = lkOrDef  (pref' "label"        ) attrskv "",
        rewApp   = lkOrDef  (pref  "applicability") attrskv "",
        complete = (lkOrDef (pref  "complete"     ) attrskv "") == "true",
        rewDesc  = lkOrDef  (pref  "desc"         ) attrskv "",
        lhs      = tkn $ lkOrDef  (pref  "formula"      ) attrskv "",
        rhs      = parseChildren (isQName' pref "rewriteRuleRHS") parseRewriteRuleRHS elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseProofRulesBlock :: Element -> ProofRulesBlock
parseProofRulesBlock elt =
    ProofRulesBlock {
        prBLabel       = lkOrDef (pref' "label") (attrToTuple $ elAttribs elt) "",
        metaVariables  = parseChildren (isQName' pref "metaVariable" ) parseMetaVariable  elt,
        inferenceRules = parseChildren (isQName' pref "inferenceRule") parseInferenceRule elt,
        rewriteRules   = parseChildren (isQName' pref "rewriteRule"  ) parseRewriteRule   elt
    }


parseTheory :: Element -> Theory
parseTheory elt =
    Theory {
        imports              = parseChildren (isQName' pref "importTheoryProject"      ) parseImportTheoryProject elt,
        typeParameters       = parseChildren (isQName' pref "typeParameter"            ) parseTypeParameter elt,
        datatypeDefinitions  = parseChildren (isQName' pref "datatypeDefinition"       ) parseDataTypeDefinition elt,
        operators            = parseChildren (isQName' pref "newOperatorDefinition"    ) parseNewOperatorDefinition elt,
        axiomaticDefinitions = parseChildren (isQName' pref "axiomaticDefinitionsBlock") parseAxiomaticDefinitionsBlock elt,
        theorems             = parseChildren (isQName' pref "theorem"                  ) parseTheorem elt,
        proofRules           = parseChildren (isQName' pref "proofRulesBlock"          ) parseProofRulesBlock elt
    }


parseTheoryFile :: String -> IO Theory
parseTheoryFile filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ Theory [] [] [] [] [] [] [])
          Just elt -> return $ parseTheory elt)







