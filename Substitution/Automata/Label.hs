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
    | Among [Token]
    | PrintsLike String
    | BeginCapture
    | EndCapture
    | No Label
    | Skip
    deriving (Eq)

isConsuming :: Label -> Bool
isConsuming Skip = False
isConsuming BeginCapture = False
isConsuming EndCapture = False
isConsuming _ = True

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
  show (Among tks      ) = show tks
  show (PrintsLike str ) = "'" ++ str ++ "'"
  show (BeginCapture   ) = "("
  show (EndCapture     ) = ")"
  show (No la          ) = "!" ++ show la
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
Among tks       //> tk'                 = tk' `elem` tks
PrintsLike str  //> tk'                 = (showAscii tk') == str || (showUTF8 tk') == str || (showTeX tk') == str
No la           //> tk'                 = not (la //> tk')




