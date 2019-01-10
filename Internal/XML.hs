{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module Internal.XML where

import Formula (Formula)
import Formula.Tokenizer (tokenize)
import Data.Set (empty)
import Data.Either (fromRight)
import RodinTheory
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)
import Data.List
import Data.List.Split (splitOn)

pref :: String -> String
pref = (++) "org.eventb.theory.core."

pref' :: String -> String
pref' = (++) "org.eventb.core."

attrToTuple :: [Attr] -> [(String,String)]
attrToTuple = map (\x -> ((qName . attrKey) x, attrVal x))

lkOrDef :: Eq a => a -> [(a,b)] -> b -> b
lkOrDef elt lst def=
    case lookup elt lst of
      Just t -> t
      Nothing -> def

lkOrDef' :: String -> [Attr] -> String -> String
lkOrDef' a attrs def = lkOrDef a (attrToTuple attrs) def

isQName :: String -> Element -> Bool
isQName name = (== name) . qName . elName

isQName' :: (String -> String) -> String -> Element -> Bool
isQName' pr name = isQName $ pr name

parseChildren :: (Element -> Bool) -> (Element -> a) -> Element -> [a]
parseChildren filt fun =
    (map fun') . (filter filt') . elContent
    where filt' (Elem elt) = filt elt
          filt' _ = False
          fun' (Elem elt) = fun elt




