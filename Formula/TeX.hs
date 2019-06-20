{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module Formula.TeX where

import Util
import Formula
import Formula.Util
import TeX
import Data.List (intercalate)
import Data.List.Split (splitWhen, splitOn)
import Data.Char (isAlpha,isAlphaNum)

instance ShowTeX Operator where
  showTeX Top = "\\top"
  showTeX Bottom = "\\bot"
  showTeX And = "\\wedge"
  showTeX Or = "\\vee"
  showTeX Implies = "\\Rightarrow"
  showTeX Equivalent = "\\Leftrightarrow"
  showTeX Not = "\\neg"
  showTeX ForAll = "\\forall"
  showTeX Exists = "\\exists"
  showTeX Equal = "="
  showTeX NotEqual = "\\neq"
  showTeX In = "\\in"
  showTeX NotIn = "\\notin"
  showTeX EmptySet = "\\emptyset"
  showTeX SubSetEq = "\\subseteq"
  showTeX NoSubSetEq = "\\not\\subseteq"
  showTeX SubSetStrict = "\\subset"
  showTeX NotSubSetStrict = "\\not\\subset"
  showTeX Union = "\\cup"
  showTeX Intersection = "\\cap"
  showTeX Difference = "\\setminus"
  showTeX Powerset = "\\mathbb{P}"
  showTeX Powerset1 = "\\mathbb{P}1"
  showTeX GenUnion = "\\bigcup"
  showTeX GenIntersection = "\\bigcap"
  showTeX Maplet = "\\mapsto"
  showTeX CartesianProduct = "\\times"
  showTeX Relation = "\\leftrightarrow"
  showTeX TotalRelation = "\\leftleftrightarrow" -- "\\leftarrow\\mkern-14mu\\leftrightarrow"
  showTeX SurjectiveRelation = "\\leftrightrightarrow" --"\\leftrightarrow\\mkern-14mu\\rightarrow"
  showTeX TotalSurjectiveRelation = "\\leftleftrightrightarrow" -- "\\leftrightarrow\\mkern-14mu\\leftrightarrow"
  showTeX DomainRestriction = "\\lhd"
  showTeX DomainSubtraction = "\\lhdminus" -- "\\lhd\\mkern-14mu-"
  showTeX RangeRestriction = "\\rhd"
  showTeX RangeSubtraction = "\\rhdminus" -- "\\rhd\\mkern-14mu-"
  showTeX RelationalForwardComposition = ";"
  showTeX RelationalBackwardComposition = "\\circ"
  showTeX RelationalOverride = "\\lhdplus" -- "\\lhd\\mkern-9mu-"
  showTeX ParallelProduct = "\\parallel"
  showTeX DirectProduct = "\\otimes"
  showTeX Inverse = "^{-1}"
  showTeX PartialFunction = "\\partialrightarrow" -- "\\mkern6mu\\mapstochar\\mkern-6mu\\rightarrow"
  showTeX TotalFunction = "\\rightarrow"
  showTeX PartialInjection = "\\partialrightarrowtail" -- "\\mkern9mu\\mapstochar\\mkern-9mu\\rightarrowtail"
  showTeX TotalInjection = "\\rightarrowtail"
  showTeX PartialSurjection = "\\partialtwoheadrightarrow" -- "\\mkern6mu\\mapstochar\\mkern-6mu\\twoheadrightarrow"
  showTeX TotalSurjection = "\\twoheadrightarrow"
  showTeX Bijection = "\\twoheadrightarrowtail" -- "\\rightarrowtail\mkern-18mu\\twoheadrightarrow"
  showTeX Lambda = "\\lambda"
  showTeX Range = ".."
  showTeX Plus = "+"
  showTeX Minus = "-"
  showTeX Multiply = "."
  showTeX Division = "\\div"
  showTeX Exponent = "\\^"
  showTeX Lower = "<"
  showTeX LowerEq = "\\leq"
  showTeX Greater = ">"
  showTeX GreaterEq = "\\geq"
  showTeX Assignment = ":="
  showTeX BeforeAfterPredicate = ":\\mid"
  showTeX SetMemberAssignment = ":\\in"
  showTeX OfType = ":"

instance ShowTeX SpecialIdent where
  showTeX Integers = "\\mathbb{Z}"
  showTeX Naturals = "\\mathbb{N}"
  showTeX Naturals1 = "\\mathbb{N}1"

instance ShowTeX Space where
  showTeX SimpleSpace = " "
  showTeX Newline = "\n"
  showTeX Tab = ind 1

instance ShowTeX SimpleToken where
  showTeX TokOpenPar = "("
  showTeX TokClosePar = ")"
  showTeX TokOpenBra = "["
  showTeX TokCloseBra = "]"
  showTeX TokOpenCBra = "{"
  showTeX TokCloseCBra = "}"
  showTeX TokComa = ","
  showTeX TokDot = "\\cdot"
  showTeX TokMid = "\\mid"

instance ShowTeX Token where
  showTeX TokUndef = ""
  showTeX (TokOp op) = showTeX op
  showTeX (TokSpecialIdent sp) = showTeX sp
  showTeX (TokOpIdent op) = "\\mathrm{" ++ op ++ "}"
  showTeX (TokIdent id) = id
  showTeX (TokSpace s) = showTeX s
  showTeX (TokToken t) = showTeX t

printTeX' :: [Token] -> String
printTeX' tks =
    print1 "" tks
    --    print1 previous current
    where print1 _  []     = ""
          print1 st (x:xs) =
              let xx = xShowTeX x
                in (space st xx) ++ xx ++ (print1 xx xs)
          space st xx =
              case st of
                '\\':_ -> if isAlphaNum $ head xx then " " else ""
                _      -> ""
          xShowTeX (TokIdent id) =
              case id of
                [] -> ""
                ('#':xs) -> xs
                (x:xs) | null xs || (not $ isAlpha x) -> id
                --_ | '"' `elem` id ->
                _ | otherwise -> "\\mathit{" ++ escape_ id ++ "}"
          xShowTeX (TokToken TokOpenCBra) = "\\{"
          xShowTeX (TokToken TokCloseCBra) = "\\}"
          xShowTeX x = showTeX x

printTeX :: [Token] -> String
printTeX tks =
    withMath False tks
    where withMath m [] = if m then "$" else ""
          withMath m (x:xs) =
              let ism = isMath x in
                  let next = withMath ism xs in
                      (if (m && ism) || ((not m) && (not ism)) then "" else "$") ++ showTeX x ++ next
          -- withMath (math mode on) (tokens)


printTeXLines'' :: Int -> [Token] -> String
printTeXLines'' = printLines (mathspace . printTeX')

printTeXLines' :: [Token] -> [String]
printTeXLines' = printLines' printTeX

printTeXLines :: Int -> [Token] -> String
printTeXLines = printLines printTeX


