module RodinTheory where

import Formula (Formula)

data NotationType = Prefix | Infix
data OperatorProp =
    OperatorProp {
        associative  :: Bool,
        commutative  :: Bool,
        formulaType  :: Bool,
        notationType :: NotationType
    }
data OperatorArgument =
    OperatorArgument {
        expression :: Formula,
        identifier :: String
    }
data OperatorWDCondition =
    OperatorWDCondition {
        predicate :: Formula
    }
data OperatorDirectDefinition =
    OperatorDirectDefinition {
        formula :: Formula
    }
data RecursiveDefinitionCase =
    RecursiveDefinitionCase {
        caseExpression :: Formula,
        caseFormula    :: Formula
    }
data OperatorRecursiveDefinition =
    OperatorRecursiveDefinition {
        inductiveArgument :: String,
        cases             :: [RecursiveDefinitionCase]
    }

data ImportTheory =
    ImportTheory {
        theory :: String
    }
data ImportTheoryProject =
    ImportTheoryProject {
        project  :: String,
        theories :: [ImportTheory]
    }

data TypeParameter =
    TypeParameter {
        typeIdentifier :: String
    }

data ConstructorArgument =
    ConstructorArgument {
        caId   :: String,
        caType :: Formula
    }
data DataTypeConstructor =
    DataTypeConstructor {
        dtcId :: String,
        args :: [ConstructorArgument]
    }
data TypeArgument =
    TypeArgument {
        typeArg :: Formula
    }
data DataTypeDefinition =
    DataTypeDefinition {
        dtId          :: String,
        typeArguments :: [TypeArgument],
        constructors  :: [DataTypeConstructor]
    }

data NewOperatorDefinition =
    NewOperatorDefinition {
        opLabel  :: String,
        opProp   :: OperatorProp,
        opArgs   :: [OperatorArgument],
        opWD     :: [OperatorWDCondition],
        opDirDef :: [OperatorDirectDefinition],
        opRecDef :: [OperatorRecursiveDefinition]
    }

data AxiomaticTypeDefinition =
    AxiomaticTypeDefinition {
        aTypeId :: String
    }
data AxiomaticOperatorDefinition =
    AxiomaticOperatorDefinition {
        aOpProp  :: OperatorProp,
        aOpLabel :: String,
        aType    :: Formula,
        aOpArgs  :: [OperatorArgument],
        aOpWD    :: [OperatorWDCondition]
    }
data AxiomaticDefinitionAxiom =
    AxiomaticDefinitionAxiom {
        aDefLabel     :: String,
        aDefPredicate :: Formula
    }
data AxiomaticDefinitionsBlock =
    AxiomaticDefinitionsBlock {
        aDefBLabel :: String,
        aDefBTypes :: [AxiomaticTypeDefinition],
        aDefBDef   :: [AxiomaticOperatorDefinition],
        aDefBAx    :: [AxiomaticDefinitionAxiom]
    }

data Theorem =
    Theorem {
        thName      :: String,
        thPredicate :: Formula
    }

data MetaVariable =
    MetaVariable {
        mvId   :: String,
        mvType :: Formula
    }
data InferenceGiven =
    InferenceGiven {
        givenPredicate :: Formula,
        givenHyp       :: Bool
    }
data InferenceInfer =
    InferenceInfer {
        inferPredicate :: Formula
    }
data InferenceRule =
    InferenceRule {
        infLabel :: String,
        infApp   :: String,
        infDesc  :: String,
        given    :: [InferenceGiven],
        infer    :: [InferenceInfer]
    }
data RewriteRuleRHS =
    RewriteRuleRHS {
        rhsLabel     :: String,
        rhsPredicate :: Formula,
        rhsFormula   :: Formula
    }
data RewriteRule =
    RewriteRule {
        rewLabel :: String,
        rewApp   :: String,
        complete :: Bool,
        rewDesc  :: String,
        lhs      :: Formula,
        rhs      :: [RewriteRuleRHS]
    }
data ProofRulesBlock =
    ProofRulesBlock {
        prBLabel       :: String,
        metaVariables  :: [MetaVariable],
        inferenceRules :: [InferenceRule],
        rewriteRules   :: [RewriteRule]
    }

data Theory =
    Theory {
        imports              :: [ImportTheoryProject],
        typeParameters       :: [TypeParameter],
        datatypeDefinitions  :: [DataTypeDefinition],
        operators            :: [NewOperatorDefinition],
        axiomaticDefinitions :: [AxiomaticDefinitionsBlock],
        theorems             :: [Theorem],
        proofRules           :: [ProofRulesBlock]
    }




