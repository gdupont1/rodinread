{-|
Module      : RodinPO.TeX
Description : Module for converting Rodin POs in LaTeX
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This module converts (/ "pretty prints") a Rodin PO file in LaTeX
-}
module RodinPO.TeX where

import Util
import TeX
import RodinPO
import Formula
import Formula.Util
import Formula.TeX
import Data.List (intercalate)

guessName :: String -> String
guessName "CTXHYP" = "Hypotheses from context:"
guessName "ABSHYP" = "Hypotheses from abstract machine:"
guessName "SEQHYP" = ""
guessName x = x 

instance ShowTeX POIdentifier where
  showTeX (POIdentifier na ty) = na ++ " " ++ (printTeX ((TokOp OfType):(TokSpace SimpleSpace):ty))

instance ShowTeX POPredicate where
  showTeX (POPredicate _ pr _) = printTeX $ removeAloneParentheses $ untype $ pr

instance ShowTeX POPredicateSet where
  showTeX (POPredicateSet na _ _ ids prs) =
      (guessName na)
      ++ (if not $ null ids then "\n    let " ++ (intercalate "\n        " $ map showTeX ids) else "")
      ++ (if not $ null prs then "\n    hyp " ++ (intercalate "\n        " $ map showTeX prs) else "")
      ++ "\n"

instance ShowTeX POSequent where
  showTeX (POSequent na _ de _ ps prs _ _) =
      na ++ ":" ++ (if not $ null de then " -- " ++ de else "")
      ++ (intercalate "" $ map showTeX ps)
      ++ "$\\vdash$\n"
      ++ "    " ++ (intercalate "\n    " $ map showTeX prs)
      ++ "\n"

instance ShowTeX POFile where
  showTeX (POFile _ prs seqs) =
      (intercalate "\n" $ map showTeX prs)
      ++ ("---\n")
      ++ (intercalate "\n" $ map showTeX seqs)



