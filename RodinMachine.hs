{-|
Module      : RodinMachine
Description : Description of a machine in Rodin
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This describes a Rodin machine, defining the Machine type.
Other submodules are responsible for other features.
-}
module RodinMachine where

import Formula (Formula)

-- | Machine refinement relationship
data RefinesMachine = RefinesMachine {
    rmTarget :: String                      -- ^ Target of the refinement (refined machine)
}

-- | Context seeing relationship
data SeesContext = SeesContext {
    scTarget :: String                      -- ^ Target of the relationship
}

-- | A variable
data Variable = Variable {
    vaIdentifier :: Formula                  -- ^ Variable's identifier
}

-- | An invariant
data Invariant = Invariant {
    invLabel :: String,                     -- ^ Invariant's label
    invPred :: Formula                      -- ^ Invariant's formulation
}

-- | A variant
data Variant = Variant {
    varExpression :: Formula                -- ^ Variant's expression
}

-- | Event refinement relationship
data RefinesEvent = RefinesEvent {
    reTarget :: String                      -- ^ Event that is being refined
}

-- | Event parameter
data Parameter = Parameter {
    paIdentifier :: Formula                  -- ^ Parameter's identifier
}

-- | Event guard
data Guard = Guard {
    guLabel :: String,                      -- ^ Guard's label
    guPred :: Formula                       -- ^ Guard's expression
}

-- | Event witness
data Witness = Witness {
    wiLabel :: Formula,                     -- ^ Witness' label
    wiPred :: Formula                       -- ^ Witness' expression
}

-- | Event action
data Action = Action {
    acLabel :: String,                      -- ^ Action's label
    acAssignment :: Formula                 -- ^ Action's expression
}

-- | Event convergence type
data ConvergenceType = Ordinary | Convergent | Anticipated deriving Enum -- 0, 1, 2

-- | An event
data Event = Event {
    evLabel       :: String,                -- ^ Event's label
    evConvergence :: ConvergenceType,       -- ^ Event's convergence type
    evExtended    :: Bool,                  -- ^ Is the event an extension of the refined event
    evRefines     :: [RefinesEvent],        -- ^ Event refinement relationship
    evParameters  :: [Parameter],           -- ^ Event's parameters
    evGuards      :: [Guard],               -- ^ Event's guards
    evWitnesses   :: [Witness],             -- ^ Event's witnesses
    evActions     :: [Action]               -- ^ Event's actions
}

-- | A machine
data Machine = Machine {
    maName       :: String,                 -- ^ Machine's name
    maRefines    :: [RefinesMachine],       -- ^ Machine refinement relationship
    maSeesCtx    :: [SeesContext],          -- ^ Machine context seeing relationship
    maVariables  :: [Variable],             -- ^ Machine's variables
    maInvariants :: [Invariant],            -- ^ Machine's invariants
    maVariants   :: [Variant],              -- ^ Machine's variant(s)
    maEvents     :: [Event]                 -- ^ Machine's events
}


