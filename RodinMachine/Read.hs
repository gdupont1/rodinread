{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module RodinMachine.Read (
        parseMachineFile
    ) where

import Formula (Formula)
import Formula.Tokenizer (tkn)
import Data.Set (empty)
import Data.Either (fromRight)
import RodinMachine
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)
import Data.List
import Data.List.Split (splitOn)
import Internal.XML

parseRefinesMachine :: Element -> RefinesMachine
parseRefinesMachine elt =
    RefinesMachine {
        rmTarget = lkOrDef (pref' "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseSeesContext :: Element -> SeesContext
parseSeesContext elt =
    SeesContext {
        scTarget = lkOrDef (pref' "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt


parseVariable :: Element -> Variable
parseVariable elt =
    Variable {
        vaIdentifier = tkn $ lkOrDef (pref' "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseInvariant :: Element -> Invariant
parseInvariant elt =
    Invariant {
        invLabel =       lkOrDef (pref' "label"    ) attrskv "",
        invPred  = tkn $ lkOrDef (pref' "predicate") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseVariant :: Element -> Variant
parseVariant elt =
    Variant {
        varExpression = tkn $ lkOrDef (pref' "expression") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt


parseRefinesEvent :: Element -> RefinesEvent
parseRefinesEvent elt =
    RefinesEvent {
        reTarget = lkOrDef (pref' "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseParameter :: Element -> Parameter
parseParameter elt =
    Parameter {
        paIdentifier = tkn $ lkOrDef (pref' "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseGuard :: Element -> Guard
parseGuard elt =
    Guard {
        guLabel =       lkOrDef (pref' "label"    ) attrskv "",
        guPred  = tkn $ lkOrDef (pref' "predicate") attrskv "" 
    }
    where attrskv = attrToTuple $ elAttribs elt

parseWitness :: Element -> Witness
parseWitness elt =
    Witness {
        wiLabel = tkn $ lkOrDef (pref' "label"    ) attrskv "",
        wiPred  = tkn $ lkOrDef (pref' "predicate") attrskv ""
    } 
    where attrskv = attrToTuple $ elAttribs elt

parseAction :: Element -> Action
parseAction elt =
    Action {
        acLabel      =       lkOrDef (pref' "label"     ) attrskv "",
        acAssignment = tkn $ lkOrDef (pref' "assignment") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseEvent :: Element -> Event
parseEvent elt =
    Event {
        evLabel       =                 lkOrDef (pref' "label"       ) attrskv "",
        evConvergence = toEnum $ read $ lkOrDef (pref' "convergence" ) attrskv "",
        evExtended    =                (lkOrDef (pref' "extended"    ) attrskv "false") == "true",
        evRefines     = parseChildren  (isQName' pref' "refinesEvent") parseRefinesEvent elt,
        evParameters  = parseChildren  (isQName' pref' "parameter"   ) parseParameter    elt,
        evGuards      = parseChildren  (isQName' pref' "guard"       ) parseGuard        elt,
        evWitnesses   = parseChildren  (isQName' pref' "witness"     ) parseWitness      elt,
        evActions     = parseChildren  (isQName' pref' "action"      ) parseAction       elt 
    }
    where attrskv = attrToTuple $ elAttribs elt

parseMachine :: Element -> Machine
parseMachine elt =
    Machine {
        maName       = "",
        maRefines    = parseChildren (isQName' pref' "refinesMachine") parseRefinesMachine elt,
        maSeesCtx    = parseChildren (isQName' pref' "seesContext"   ) parseSeesContext    elt,
        maVariables  = parseChildren (isQName' pref' "variable"      ) parseVariable       elt,
        maInvariants = parseChildren (isQName' pref' "invariant"     ) parseInvariant      elt,
        maVariants   = parseChildren (isQName' pref' "variant"       ) parseVariant        elt,
        maEvents     = parseChildren (isQName' pref' "event"         ) parseEvent          elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseMachineFile :: String -> IO Machine
parseMachineFile filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ Machine "??" [] [] [] [] [] [])
          Just elt -> return $ (parseMachine elt) { maName = getName filename })
    where getName = (head . splitOn ".") . (last . splitOn "/")





