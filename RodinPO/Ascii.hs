{-|
Module      : RodinPO.Ascii
Description : Module for converting Rodin POs in ASCII
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This module converts (/ "pretty prints") a Rodin PO file in ASCII
-}
module RodinPO.Ascii where

import Util
import Ascii
import RodinPO
import Formula
import Formula.Util
import Formula.Ascii
import Data.List (intercalate)

guessName :: String -> String
guessName "CTXHYP" = "Hypotheses from context:"
guessName "ABSHYP" = "Hypotheses from abstract machine:"
guessName "SEQHYP" = ""
guessName x = x

instance ShowAscii POIdentifier where
  showAscii (POIdentifier na ty) = na ++ " " ++ (printAscii ((TokOp OfType):(TokSpace SimpleSpace):ty))

instance ShowAscii POPredicate where
  showAscii (POPredicate _ pr _) = printAscii pr

instance ShowAscii POPredicateSet where
  showAscii (POPredicateSet na _ _ ids prs) =
      (guessName na)
      ++ (if not $ null ids then "\n    let " ++ (intercalate "\n        " $ map showAscii ids) else "")
      ++ (if not $ null prs then "\n    hyp " ++ (intercalate "\n        " $ map showAscii prs) ++ "\n" else "")

instance ShowAscii POSequent where
  showAscii (POSequent na _ de _ ps prs _ _) =
      na ++ ":" ++ (if not $ null de then " -- " ++ de else "")
      ++ (intercalate "" $ map showAscii ps)
      ++ "|-\n"
      ++ "    " ++ (intercalate "\n    " $ map showAscii prs)
      ++ "\n"

instance ShowAscii POFile where
  showAscii (POFile _ prs seqs) =
      (intercalate "\n" $ map showAscii prs)
      ++ ("---\n")
      ++ (intercalate "\n" $ map showAscii seqs)



