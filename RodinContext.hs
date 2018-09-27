module RodinContext where

import Formula (Formula)

data ExtendsContext = ExtendsContext {
    ecTarget :: String
} 

data Axiom = Axiom {
    axLabel :: String,
    axPred :: Formula,
    axIsTheorem :: Bool
}

data Constant = Constant {
    ctName :: String
}

data CarrierSet = CarrierSet {
    csName :: String
}

data Context = Context {
    ctxName      :: String,
    ctxExtends   :: [ExtendsContext],
    ctxAxioms    :: [Axiom],
    ctxConstants :: [Constant],
    ctxSets      :: [CarrierSet]
}


