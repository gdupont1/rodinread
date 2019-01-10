{-|
Module      : RodinContext.TeX
Description : Module for converting a Rodin context in LaTeX
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This module converts (/ "pretty prints") a Rodin context in LaTeX.
-}
module RodinContext.TeX where

import Util
import TeX
import RodinContext
import Formula
import Formula.Util
import Formula.TeX

instance ShowTeX ExtendsContext where
  showTeX (ExtendsContext tg) = "\n" ++ ind 1 ++ tg

instance ShowTeX Axiom where
  showTeX (Axiom lb pr _) =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines 2 else printTeX) pr

instance ShowTeX Constant where
  showTeX (Constant nm) =
      "\n" ++ ind 1 ++ nm 

instance ShowTeX CarrierSet where
  showTeX (CarrierSet nm) =
      "\n" ++ ind 1 ++ nm

instance ShowTeX Context where
  showTeX (Context nm ec ax ct cs) =
      "CONTEXT\n" ++ ind 1 ++ nm ++
      (if not $ null ec then "\n" ++ "EXTENDS"   ++ (texlist "" ec) else "") ++
      (if not $ null cs then "\n" ++ "SETS"      ++ (texlist "" cs) else "") ++
      (if not $ null ct then "\n" ++ "CONSTANTS" ++ (texlist "" ct) else "") ++
      (if not $ null ax then "\n" ++ "AXIOMS"    ++ (texlist "" ax) else "") ++
      "\nEND"



