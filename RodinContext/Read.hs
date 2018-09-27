module RodinContext.Read (
        parseContextFile
    ) where

import Formula (Formula)
import Formula.Tokenizer (tkn)
import Data.Set (empty)
import Data.Either (fromRight)
import RodinContext
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)
import Data.List
import Data.List.Split (splitOn)
import Internal.XML

parseExtendsContext :: Element -> ExtendsContext
parseExtendsContext elt =
    ExtendsContext {
        ecTarget = lkOrDef (pref' "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseAxiom :: Element -> Axiom
parseAxiom elt =
    Axiom {
        axLabel     =       lkOrDef (pref' "label")     attrskv "",
        axPred      = tkn $ lkOrDef (pref' "predicate") attrskv "",
        axIsTheorem =      (lkOrDef (pref' "theorem")   attrskv "false") == "true"
    }
    where attrskv = attrToTuple $ elAttribs elt

parseConstant :: Element -> Constant
parseConstant elt =
    Constant {
        ctName =       lkOrDef (pref' "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseCarrierSet :: Element -> CarrierSet
parseCarrierSet elt =
    CarrierSet {
        csName =       lkOrDef (pref' "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseContext :: Element -> Context
parseContext elt =
    Context {
        ctxName      = "",
        ctxExtends   = parseChildren (isQName' pref' "extendsContext") parseExtendsContext elt,
        ctxAxioms    = parseChildren (isQName' pref' "axiom"         ) parseAxiom          elt,
        ctxConstants = parseChildren (isQName' pref' "constant"      ) parseConstant       elt,
        ctxSets      = parseChildren (isQName' pref' "carrierSet"    ) parseCarrierSet     elt
    }

parseContextFile :: String -> IO Context
parseContextFile filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ Context "??" [] [] [] [])
          Just elt -> return $ (parseContext elt) { ctxName = getName filename })
    where getName = (head . splitOn ".") . (last . splitOn "/")




