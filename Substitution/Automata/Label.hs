module Substitution.Automata.Label where

import Ascii
import TeX
import Formula
import Formula.Ascii
import Formula.UTF8
import Formula.TeX

data Label =
      Never
    | Always
    | Undef
    | AnyToken
    | AnyOperator
    | AnySpecialIdent
    | AnyOpIdent
    | AnyIdent
    | AnySpace
    | AnySimpleToken
    | Exactly Token
    | PrintsLike String
    | BeginCapture
    | EndCapture
    | No Label
    | Skip
    deriving (Eq)

instance Ord Label where
  compare a b
      | a == b = EQ
      | isConsuming a && isConsuming b = EQ
      | isConsuming a = LT
      | isConsuming b = GT
      | isGeneral a && isGeneral b = EQ
      | isGeneral a = LT
      | isGeneral b = GT
      | No _ <- a, No _ <- b = EQ
      | No _ <- a = LT
      | No _ <- b = GT
      | isAny a && isAny b = EQ
      | isAny a = LT
      | isAny b = GT
      | otherwise = EQ


isConsuming :: Label -> Bool
isConsuming Skip = False
isConsuming BeginCapture = False
isConsuming EndCapture = False
isConsuming _ = True

isGeneral :: Label -> Bool
isGeneral Never = True
isGeneral Always = True
isGeneral Undef = True
isGeneral _ = False

isAny :: Label -> Bool
isAny AnyToken        = True
isAny AnyOperator     = True
isAny AnySpecialIdent = True
isAny AnyOpIdent      = True
isAny AnyIdent        = True
isAny AnySpace        = True
isAny AnySimpleToken  = True
isAny _               = False

isSkip :: Label -> Bool
isSkip Skip = True
--isSkip EndCapture = True
isSkip _ = False

instance Show Label where
  show (Never          ) = "!" 
  show (Always         ) = "_"
  show (Undef          ) = "??"
  show (AnyToken       ) = "."
  show (AnyOperator    ) = "<op>"
  show (AnySpecialIdent) = "<sid>"
  show (AnyOpIdent     ) = "<opid>"
  show (AnyIdent       ) = "<id>"
  show (AnySpace       ) = "<space>"
  show (AnySimpleToken ) = "<simple>"
  show (Exactly tk     ) = show tk
  show (PrintsLike str ) = "'" ++ str ++ "'"
  show (BeginCapture   ) = "("
  show (EndCapture     ) = ")"
  show (No la          ) = "¬" ++ show la
  show (Skip           ) = ">>>"

infix 8 //>

(//>) :: Label -> Token -> Bool

Never           //> _                   = False
Skip            //> _                   = False
Always          //> _                   = True
Undef           //> TokUndef            = True
Undef           //> _                   = False
_               //> TokUndef            = False
BeginCapture    //> _                   = True
EndCapture      //> _                   = True
AnyToken        //> _                   = True
AnyOperator     //> (TokOp _)           = True
AnyOperator     //> _                   = False
AnySpecialIdent //> (TokSpecialIdent _) = True
AnySpecialIdent //> _                   = False
AnyOpIdent      //> (TokOpIdent _)      = True
AnyOpIdent      //> _                   = False
AnyIdent        //> (TokIdent _)        = True
AnyIdent        //> _                   = False
AnySpace        //> (TokSpace _)        = True
AnySpace        //> _                   = False
AnySimpleToken  //> (TokToken _)        = True
AnySimpleToken  //> _                   = False
Exactly tk      //> tk'                 = tk == tk'
PrintsLike str  //> tk'                 = (showAscii tk') == str || (showUTF8 tk') == str || (showTeX tk') == str
No la           //> tk'                 = not (la //> tk')




