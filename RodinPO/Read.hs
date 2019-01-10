{-|
Module      : RodinPO.Read
Description : Rodin PO file reader
Copyright   : (c) Guillaume Dupont, 2019
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

Module for reading a Rodin PO file (.bpo) as an XML File.
-}
module RodinPO.Read where

import Formula (Formula)
import Formula.Tokenizer (tkn)
import Data.Set (empty)
import Data.Either (fromRight)
import RodinPO
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)
import Data.List
import Data.List.Split (splitOn)
import Internal.XML

parsePOIdentifier :: Element -> POIdentifier
parsePOIdentifier elt =
    POIdentifier {
        poiName =       lkOrDef        "name"  attrskv "",
        poiType = tkn $ lkOrDef (pref' "type") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOPredicate :: Element -> POPredicate
parsePOPredicate elt =
    POPredicate {
        popName      =       lkOrDef        "name"       attrskv "",
        popPredicate = tkn $ lkOrDef (pref' "predicate") attrskv "",
        popSource    =       lkOrDef (pref' "source")    attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOPredicateSet :: Element -> POPredicateSet
parsePOPredicateSet elt =
    POPredicateSet {
        popsName        = lkOrDef        "name"       attrskv "",
        popsParentSet   = lkOrDef (pref' "parentSet") attrskv "",
        popsStamp       = lkOrDef (pref' "poStamp"  ) attrskv "",
        popsIdentifiers = parseChildren (isQName' pref' "poIdentifier") parsePOIdentifier elt,
        popsPredicates  = parseChildren (isQName' pref' "poPredicate" ) parsePOPredicate  elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOSource :: Element -> POSource
parsePOSource elt = POSource {
        posrcName   = lkOrDef        "name"    attrskv "",
        posrcRole   = lkOrDef (pref' "poRole") attrskv "",
        posrcSource = lkOrDef (pref' "source") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOSelHint :: Element -> POSelHint 
parsePOSelHint elt = POSelHint {
        poshName   = lkOrDef        "name"          attrskv "",
        poshFirst  = lkOrDef (pref' "poSelHintFst") attrskv "",
        poshSecond = lkOrDef (pref' "poSelHintSnd") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOSequent :: Element -> POSequent
parsePOSequent elt = POSequent {
        posName          =  lkOrDef        "name"      attrskv "",
        posAccurate      = (lkOrDef (pref' "accurate") attrskv "false") == "true",
        posDesc          =  lkOrDef (pref' "poDesc"  ) attrskv "",
        posStamp         =  lkOrDef (pref' "poStamp" ) attrskv "",
        posPredicateSets = parseChildren (isQName' pref' "poPredicateSet") parsePOPredicateSet elt,
        posPredicates    = parseChildren (isQName' pref' "poPredicate"   ) parsePOPredicate    elt,
        posSources       = parseChildren (isQName' pref' "poSource"      ) parsePOSource       elt,
        posSelHints      = parseChildren (isQName' pref' "poSelHint"     ) parsePOSelHint      elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOFile :: Element -> POFile
parsePOFile elt = POFile {
        pofStamp         = lkOrDef (pref' "poStamp" ) attrskv "",
        pofPredicateSets = parseChildren (isQName' pref' "poPredicateSet") parsePOPredicateSet elt,
        pofSequents      = parseChildren (isQName' pref' "poSequent"     ) parsePOSequent      elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePOFileFile :: String -> IO POFile
parsePOFileFile filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ POFile "" [] [])
          Just elt -> return $ parsePOFile elt)



