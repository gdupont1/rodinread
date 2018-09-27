module Formula where


data Operator =
      Top
    | Bottom
    | And
    | Or
    | Implies
    | Equivalent
    | Not
    | ForAll
    | Exists
    | Equal
    | NotEqual
    | In
    | NotIn
    | EmptySet
    | SubSetEq
    | NoSubSetEq
    | SubSetStrict
    | NotSubSetStrict
    | Union
    | Intersection
    | Difference
    | Powerset
    | Powerset1
    | GenUnion
    | GenIntersection
    | Maplet
    | CartesianProduct
    | Relation
    | TotalRelation
    | SurjectiveRelation
    | TotalSurjectiveRelation
    | DomainRestriction
    | DomainSubtraction
    | RangeRestriction
    | RangeSubtraction
    | RelationalForwardComposition
    | RelationalBackwardComposition
    | RelationalOverride
    | ParallelProduct
    | DirectProduct
    | Inverse
    | PartialFunction
    | TotalFunction
    | PartialInjection
    | TotalInjection
    | PartialSurjection
    | TotalSurjection
    | Bijection
    | Lambda
    | Range
    | Plus
    | Minus
    | Multiply
    | Division
    | Exponent
    | Lower
    | LowerEq
    | Greater
    | GreaterEq
    | Assignment
    | BeforeAfterPredicate
    | SetMemberAssignment
    | OfType
    deriving (Eq,Ord,Enum,Show)

data SpecialIdent =
      Integers
    | Naturals
    | Naturals1
    deriving (Eq,Ord,Enum,Show)

data Space =
      SimpleSpace
    | Newline
    | Tab
    deriving (Eq,Ord,Enum,Show)

data SimpleToken =
      TokOpenPar
    | TokClosePar
    | TokOpenBra
    | TokCloseBra
    | TokOpenCBra
    | TokCloseCBra
    | TokComa
    | TokDot
    | TokMid
    deriving (Eq,Ord,Enum,Show)

data Token =
      TokUndef
    | TokOp Operator
    | TokSpecialIdent SpecialIdent
    | TokOpIdent String
    | TokIdent String
    | TokSpace Space
    | TokToken SimpleToken
    deriving Show

type Formula = [Token]



