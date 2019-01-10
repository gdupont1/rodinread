{-|
Module      : Formula
Description : module for representing a general Event-B formula
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This top level module mainly contains the datatypes representing a formula.
Submodules contains other functionalities like tokenization or printing.
-}
module Formula where

-- | An Event-B operator
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

-- | An reserved identifier that should be represented by a special symbol
data SpecialIdent =
      Integers
    | Naturals
    | Naturals1
    deriving (Eq,Ord,Enum,Show)

-- | Some kind of white space
data Space =
      SimpleSpace
    | Newline
    | Tab
    deriving (Eq,Ord,Enum,Show)

-- | Other types of tokens, mainly parentheses/brackets, comas, dot and mid (used as delimiters)
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

-- | Main type : a token in the formula
data Token =
      TokUndef                         -- ^ Undefined token
    | TokOp Operator                   -- ^ Operator
    | TokSpecialIdent SpecialIdent     -- ^ Special identifier
    | TokOpIdent String                -- ^ Identifier corresponding to an operator
    | TokIdent String                  -- ^ Any other identifier
    | TokSpace Space                   -- ^ A white space
    | TokToken SimpleToken             -- ^ Some other "top-level" token
    deriving Show

-- | A formula is a list of tokens
type Formula = [Token]



