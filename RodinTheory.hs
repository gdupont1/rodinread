{-|
Module      : RodinTheory
Description : Description of a theory in Rodin (as a syntactical tree)
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This describes a Rodin theory, defining the Theory type.
Other submodules are responsible for other features.
-}
module RodinTheory where

import Formula (Formula)

-- | Operator notation type
data NotationType = Prefix | Infix

-- | Operator properties: intermediate representation used in the operator definition
data OperatorProp =
    OperatorProp {
        associative  :: Bool,                               -- ^ Is the operator associative
        commutative  :: Bool,                               -- ^ Is the operator commutative
        formulaType  :: Bool,                               -- ^ Is the operator an expression or a predicate
        notationType :: NotationType                        -- ^ Operator's notation type
    }
-- | An operator argument
data OperatorArgument =
    OperatorArgument {
        expression :: Formula,                              -- ^ Operator's type
        identifier :: String                                -- ^ Operator's identifier
    }
-- | An operator well-definedness condition
data OperatorWDCondition =
    OperatorWDCondition {
        predicate :: Formula                                -- ^ Well-definedness predicate
    }
-- | Operator's direct definition
data OperatorDirectDefinition =
    OperatorDirectDefinition {
        formula :: Formula                                  -- ^ Direct definition formula
    }
-- | Operator's recursive definition's case
data RecursiveDefinitionCase =
    RecursiveDefinitionCase {
        caseExpression :: Formula,                          -- ^ Case's expression (what it matches against)
        caseFormula    :: Formula                           -- ^ Case's formula (its definition)
    }
-- | Operator's recursive definition
data OperatorRecursiveDefinition =
    OperatorRecursiveDefinition {
        inductiveArgument :: String,                        -- ^ Recursive definition's inductive argument (what should be matched)
        cases             :: [RecursiveDefinitionCase]      -- ^ List of cases
    }

-- | Theory import
data ImportTheory =
    ImportTheory {
        theory :: String                                    -- ^ Import target
    }
-- | Theory project import
data ImportTheoryProject =
    ImportTheoryProject {
        project  :: String,                                 -- ^ Import target (project name)
        theories :: [ImportTheory]                          -- ^ Theories to be imported from that project
    }

-- | Theory's type parameter
data TypeParameter =
    TypeParameter {
        typeIdentifier :: String                            -- ^ Type's identifier
    }

-- | Data type construtor argument
data ConstructorArgument =
    ConstructorArgument {
        caId   :: String,                                   -- ^ Argument's identifier
        caType :: Formula                                   -- ^ Argument's type
    }
-- | Data type constructor
data DataTypeConstructor =
    DataTypeConstructor {
        dtcId :: String,                                    -- ^ Constructor's identifier
        args :: [ConstructorArgument]                       -- ^ Constructor's arguments
    }
-- | Data type type argument
data TypeArgument =
    TypeArgument {
        typeArg :: Formula                                  -- ^ Type formula
    }
-- | Data type definition
data DataTypeDefinition =
    DataTypeDefinition {
        dtId          :: String,                            -- ^ Data type identifier
        typeArguments :: [TypeArgument],                    -- ^ Data type type arguments
        constructors  :: [DataTypeConstructor]              -- ^ Data type constructors
    }

-- | Operator definition
data NewOperatorDefinition =
    NewOperatorDefinition {
        opLabel  :: String,                                 -- ^ 
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




