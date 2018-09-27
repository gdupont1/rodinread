module Formula.Ascii where

import Formula
import Formula.Util
import Util
import Ascii
import Data.List (intercalate)
import Data.List.Split (splitWhen, splitOn)

instance ShowAscii Operator where
  showAscii Top                           = "TRUE"
  showAscii Bottom                        = "FALSE"
  showAscii And                           = "&"
  showAscii Or                            = "or"
  showAscii Implies                       = "=>"
  showAscii Equivalent                    = "<=>"
  showAscii Not                           = "not"
  showAscii ForAll                        = "!"
  showAscii Exists                        = "#"
  showAscii Equal                         = "="
  showAscii NotEqual                      = "/="
  showAscii In                            = ":"
  showAscii NotIn                         = "/:"
  showAscii EmptySet                      = "{}"
  showAscii SubSetEq                      = "<:"
  showAscii NoSubSetEq                    = "/<:"
  showAscii SubSetStrict                  = "<<:"
  showAscii NotSubSetStrict               = "/<<:"
  showAscii Union                         = "\\/"
  showAscii Intersection                  = "/\\"
  showAscii Difference                    = "\\"
  showAscii Powerset                      = "POW"
  showAscii Powerset1                     = "POW1"
  showAscii GenUnion                      = "UNION"
  showAscii GenIntersection               = "INTER"
  showAscii Maplet                        = "|->"
  showAscii CartesianProduct              = "*"
  showAscii Relation                      = "<->"
  showAscii TotalRelation                 = "<<->" 
  showAscii SurjectiveRelation            = "<->>"
  showAscii TotalSurjectiveRelation       = "<<->>"
  showAscii DomainRestriction             = "<|"
  showAscii DomainSubtraction             = "<<|"
  showAscii RangeRestriction              = "|>"
  showAscii RangeSubtraction              = "|>>"
  showAscii RelationalForwardComposition  = ";"
  showAscii RelationalBackwardComposition = "circ"
  showAscii RelationalOverride            = "<+"
  showAscii ParallelProduct               = "||"
  showAscii DirectProduct                 = "><"
  showAscii Inverse                       = "~"
  showAscii PartialFunction               = "+->"
  showAscii TotalFunction                 = "-->"
  showAscii PartialInjection              = ">+>"
  showAscii TotalInjection                = ">->"
  showAscii PartialSurjection             = "+->>"
  showAscii TotalSurjection               = "-->>"
  showAscii Bijection                     = ">->>"
  showAscii Lambda                        = "%"
  showAscii Range                         = ".."
  showAscii Plus                          = "+"
  showAscii Minus                         = "-"
  showAscii Multiply                      = "*"
  showAscii Division                      = "/"
  showAscii Exponent                      = "^"
  showAscii Lower                         = "<"
  showAscii LowerEq                       = "<="
  showAscii Greater                       = ">"
  showAscii GreaterEq                     = ">="
  showAscii Assignment                    = ":="
  showAscii BeforeAfterPredicate          = ":|"
  showAscii SetMemberAssignment           = "::"
  showAscii OfType                        = ":"

instance ShowAscii SpecialIdent where
  showAscii Integers = "INTEGER"
  showAscii Naturals = "NATURAL"
  showAscii Naturals1 = "NATURAL1"

instance ShowAscii Space where
  showAscii SimpleSpace = " "
  showAscii Newline = "\n"
  showAscii Tab = ind 1

instance ShowAscii SimpleToken where
  showAscii TokOpenPar = "("
  showAscii TokClosePar = ")"
  showAscii TokOpenBra = "["
  showAscii TokCloseBra = "]"
  showAscii TokOpenCBra = "{"
  showAscii TokCloseCBra = "}"
  showAscii TokComa = ","
  showAscii TokDot = "."
  showAscii TokMid = "|"

instance ShowAscii Token where
  showAscii TokUndef = ""
  showAscii (TokOp op) = showAscii op
  showAscii (TokSpecialIdent sp) = showAscii sp
  showAscii (TokOpIdent op) = op
  showAscii (TokIdent id) = id
  showAscii (TokSpace s) = showAscii s
  showAscii (TokToken t) = showAscii t


printAscii :: [Token] -> String
printAscii = foldl (\acc -> \x -> acc ++ (showAscii x)) ""

printAsciiLines' :: [Token] -> [String]
printAsciiLines' = printLines' printAscii

printAsciiLines :: Int -> [Token] -> String
printAsciiLines = printLines printAscii



