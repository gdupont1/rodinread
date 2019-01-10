{-|
Module      : RodinPO
Description : Description of a proof obligation (PO) file in Rodin
Copyright   : (c) Guillaume Dupont, 2019
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This describes a Rodin PO file, holding sequents that must be
proven.
Other submodules are responsible for other features.
-}
module RodinPO where

import Formula (Formula)

data POIdentifier = POIdentifier {
    poiName :: String,
    poiType :: Formula
}

data POPredicate = POPredicate {
    popName :: String,
    popPredicate :: Formula,
    popSource :: String
}

data POPredicateSet = POPredicateSet {
    popsName :: String,
    popsParentSet :: String,
    popsStamp :: String,
    popsIdentifiers :: [POIdentifier],
    popsPredicates :: [POPredicate]
}

data POSource = POSource {
    posrcName :: String,
    posrcRole :: String,
    posrcSource :: String
}

data POSelHint = POSelHint {
    poshName :: String,
    poshFirst :: String,
    poshSecond :: String
}

data POSequent = POSequent {
    posName :: String,
    posAccurate :: Bool,
    posDesc :: String,
    posStamp :: String,
    posPredicateSets :: [POPredicateSet],
    posPredicates :: [POPredicate],
    posSources :: [POSource],
    posSelHints :: [POSelHint]
}

data POFile = POFile {
   pofStamp :: String,
   pofPredicateSets :: [POPredicateSet],
   pofSequents :: [POSequent]
}


