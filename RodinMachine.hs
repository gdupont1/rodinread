module RodinMachine where

import Formula (Formula)

data RefinesMachine = RefinesMachine {
    rmTarget :: String
}

data SeesContext = SeesContext {
    scTarget :: String
}

data Variable = Variable {
    vaIdentifier :: String
}

data Invariant = Invariant {
    invLabel :: String,
    invPred :: Formula
}

data Variant = Variant {
    varExpression :: Formula
}

data RefinesEvent = RefinesEvent {
    reTarget :: String
}

data Parameter = Parameter {
    paIdentifier :: String
}

data Guard = Guard {
    guLabel :: String,
    guPred :: Formula
}

data Witness = Witness {
    wiLabel :: String,
    wiPred :: Formula
}

data Action = Action {
    acLabel :: String,
    acAssignment :: Formula
}

data ConvergenceType = Ordinary | Convergent | Anticipated deriving Enum -- 0, 1, 2

data Event = Event {
    evLabel       :: String,
    evConvergence :: ConvergenceType,
    evExtended    :: Bool,
    evRefines     :: [RefinesEvent],
    evParameters  :: [Parameter],
    evGuards      :: [Guard],
    evWitnesses   :: [Witness],
    evActions     :: [Action]
}

data Machine = Machine {
    maName       :: String,
    maRefines    :: [RefinesMachine],
    maSeesCtx    :: [SeesContext],
    maVariables  :: [Variable],
    maInvariants :: [Invariant],
    maVariants   :: [Variant],
    maEvents     :: [Event]
}


