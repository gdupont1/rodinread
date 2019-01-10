{-|
Module      : RodinContext
Description : Description of a context in Rodin (as a syntactical tree)
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This describes a Rodin context, defining the Context type.
Other submodules are responsible for other features.
-}
module RodinContext where

import Formula (Formula)

-- | Extends Context relationship
data ExtendsContext = ExtendsContext {
    ecTarget :: String      -- ^ Which context is being extended
} 

-- | Axiom definition
data Axiom = Axiom {
    axLabel :: String,      -- ^ Axiom's label
    axPred :: Formula,      -- ^ Axiom's predicate
    axIsTheorem :: Bool     -- ^ Is the axiom a theorem
}

-- | Constant definition
data Constant = Constant {
    ctName :: String        -- ^ Constant's name
}

-- | Carrier set definition
data CarrierSet = CarrierSet {
    csName :: String        -- ^ Set's name
}

-- | Context definition
data Context = Context {
    ctxName      :: String,                 -- ^ Context name
    ctxExtends   :: [ExtendsContext],       -- ^ Context extension relationships
    ctxAxioms    :: [Axiom],                -- ^ Axioms
    ctxConstants :: [Constant],             -- ^ Constants
    ctxSets      :: [CarrierSet]            -- ^ Carrier sets
}


