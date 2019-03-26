{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module RodinMachine.TeX where

import Util
import TeX
import RodinMachine
import Formula
import Formula.Util
import Formula.TeX

instance ShowTeX RefinesMachine where
  showTeX (RefinesMachine r) = "\n" ++ ind 1 ++ r

instance ShowTeX SeesContext where
  showTeX (SeesContext c) = "\n" ++ ind 1 ++ c

instance ShowTeX Variable where
  showTeX (Variable v) = "\n" ++ ind 1 ++ v

instance ShowTeX Invariant where
  showTeX (Invariant lb pr) =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 2 else math . printTeX') pr

instance ShowTeX Variant where
  showTeX (Variant ex) =
      tail $ printTeXLines'' 1 ex

instance ShowTeX RefinesEvent where
  showTeX (RefinesEvent ev) = ev

instance ShowTeX Parameter where
  showTeX (Parameter pa) = "\n" ++ ind 2 ++ pa

instance ShowTeX Guard where
  showTeX (Guard lb pr) =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 3 else math . printTeX') pr

instance ShowTeX Witness where
  showTeX (Witness lb pr) =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 3 else math . printTeX') pr

instance ShowTeX Action where
  showTeX (Action lb pr) =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 3 else math . printTeX') pr

instance ShowTeX ConvergenceType where
  showTeX Ordinary = ""
  showTeX Convergent = " <convergent>"
  showTeX Anticipated = " <anticipated>"

instance ShowTeX Event where
  showTeX (Event lb co _ re pa gu wi ac) =
      "\n" ++ ind 1 ++ lb ++ showTeX co ++
      (if not $ null re then "\n" ++ ind 1 ++ "REFINES " ++ (texlist "," re) else "") ++
      (if not $ null pa then "\n" ++ ind 1 ++ "ANY"      ++ (texlist ""  pa) else "") ++
      (if not $ null gu then "\n" ++ ind 1 ++ "WHERE"    ++ (texlist ""  gu) else "") ++
      (if not $ null wi then "\n" ++ ind 1 ++ "WITH"     ++ (texlist ""  wi) else "") ++
      (if not $ null ac then "\n" ++ ind 1 ++ "THEN"     ++ (texlist ""  ac) else "") ++
      "\n" ++ ind 1 ++ "END\n"

instance ShowTeX Machine where
  showTeX (Machine na re se va inv var ev) =
      "MACHINE\n" ++ ind 1 ++ na ++
      (if not $ null re  then "\n" ++ "REFINES"    ++ (texlist "" re ) else "") ++
      (if not $ null se  then "\n" ++ "SEES"       ++ (texlist "" se ) else "") ++
      (if not $ null va  then "\n" ++ "VARIABLES"  ++ (texlist "" va ) else "") ++
      (if not $ null inv then "\n" ++ "INVARIANTS" ++ (texlist "" inv) else "") ++
      (if not $ null var then "\n" ++ "VARIANT"    ++ (texlist "" var) else "") ++
      (if not $ null ev  then "\n" ++ "EVENTS"     ++ (texlist "" ev ) else "") ++
      "\nEND"



