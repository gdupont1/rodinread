module Formula.UTF8 where

import Formula

class ShowUTF8 a where
  showUTF8 :: a -> String

instance ShowUTF8 Operator where
  showUTF8 Top = "\x22a4"
  showUTF8 Bottom = "\x22a5"
  showUTF8 And = "\x2227"
  showUTF8 Or = "\x2228"
  showUTF8 Implies = "\x21d2"
  showUTF8 Equivalent = "\x21d4"
  showUTF8 Not = "¬"
  showUTF8 ForAll = "\x2200"
  showUTF8 Exists = "\x2203"
  showUTF8 Equal = "="
  showUTF8 NotEqual = "\x2260"
  showUTF8 In = "\x2208"
  showUTF8 NotIn = "\x2209"
  showUTF8 EmptySet = "\x2205"
  showUTF8 SubSetEq = "\x2286"
  showUTF8 NoSubSetEq = "\x2288"
  showUTF8 SubSetStrict = "\x2282"
  showUTF8 NotSubSetStrict = "\x2284"
  showUTF8 Union = "\x222a"
  showUTF8 Intersection = "\x2229"
  showUTF8 Difference = "\x2216"
  showUTF8 Powerset = "\x2119"
  showUTF8 Powerset1 = "\x2119\&1"
  showUTF8 GenUnion = "\x22c3"
  showUTF8 GenIntersection = "\x22c2"
  showUTF8 Maplet = "\x21a6"
  showUTF8 CartesianProduct = "\xd7"
  showUTF8 Relation = "\x2194"
  showUTF8 TotalRelation = "\xe100" -- "\\leftarrow\\mkern-14mu\\leftrightarrow"
  showUTF8 SurjectiveRelation = "\xe101" --"\\leftrightarrow\\mkern-14mu\\rightarrow"
  showUTF8 TotalSurjectiveRelation = "\xe102" -- "\\leftrightarrow\\mkern-14mu\\leftrightarrow"
  showUTF8 DomainRestriction = "\x25c1"
  showUTF8 DomainSubtraction = "\x2a64" -- "\\lhd\\mkern-14mu-"
  showUTF8 RangeRestriction = "\x25b7"
  showUTF8 RangeSubtraction = "\x2a65" -- "\\rhd\\mkern-14mu-"
  showUTF8 RelationalForwardComposition = ";"
  showUTF8 RelationalBackwardComposition = "\x2218"
  showUTF8 RelationalOverride = "\xe103" -- "\\lhd\\mkern-9mu-"
  showUTF8 ParallelProduct = "\x2225"
  showUTF8 DirectProduct = "\x2297"
  showUTF8 Inverse = "\x223c"
  showUTF8 PartialFunction = "\x21f8" -- "\\mkern6mu\\mapstochar\\mkern-6mu\\rightarrow"
  showUTF8 TotalFunction = "\x2192"
  showUTF8 PartialInjection = "\x2914" -- "\\mkern9mu\\mapstochar\\mkern-9mu\\rightarrowtail"
  showUTF8 TotalInjection = "\x21a3"
  showUTF8 PartialSurjection = "\x2900" -- "\\mkern6mu\\mapstochar\\mkern-6mu\\twoheadrightarrow"
  showUTF8 TotalSurjection = "\x21a0"
  showUTF8 Bijection = "\x2916" -- "\\rightarrowtail\mkern-18mu\\twoheadrightarrow"
  showUTF8 Lambda = "\x03bb"
  showUTF8 Range = "\x2025"
  showUTF8 Plus = "+"
  showUTF8 Minus = "\x2212"
  showUTF8 Multiply = "\x2217"
  showUTF8 Division = "\x00f7"
  showUTF8 Exponent = "^"
  showUTF8 Lower = "<"
  showUTF8 LowerEq = "\x2264"
  showUTF8 Greater = ">"
  showUTF8 GreaterEq = "\x2265"
  showUTF8 Assignment = "\x2254"
  showUTF8 BeforeAfterPredicate = ":\x2223"
  showUTF8 SetMemberAssignment = ":\x2208"
  showUTF8 OfType = "\x2982"

instance ShowUTF8 SpecialIdent where
  showUTF8 Integers = "\x2124"
  showUTF8 Naturals = "\x2115"
  showUTF8 Naturals1 = "\x2115\&1"

instance ShowUTF8 Space where
  showUTF8 SimpleSpace = " "
  showUTF8 Newline = "\n"
  showUTF8 Tab = "\t"

instance ShowUTF8 SimpleToken where
  showUTF8 TokOpenPar = "("
  showUTF8 TokClosePar = ")"
  showUTF8 TokOpenBra = "["
  showUTF8 TokCloseBra = "]"
  showUTF8 TokOpenCBra = "{"
  showUTF8 TokCloseCBra = "}"
  showUTF8 TokComa = ","
  showUTF8 TokDot = "\x00b7"
  showUTF8 TokMid = "\x2223"

instance ShowUTF8 Token where
  showUTF8 TokUndef = ""
  showUTF8 (TokOp op) = showUTF8 op
  showUTF8 (TokSpecialIdent sp) = showUTF8 sp
  showUTF8 (TokOpIdent op) = op
  showUTF8 (TokIdent id) = id
  showUTF8 (TokSpace s) = showUTF8 s
  showUTF8 (TokToken t) = showUTF8 t



