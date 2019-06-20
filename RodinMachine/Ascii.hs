{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module RodinMachine.Ascii where

import Util
import Ascii
import RodinMachine
import Formula
import Formula.Util
import Formula.Ascii

instance ShowAscii RefinesMachine where
  showAscii (RefinesMachine r) = "\n" ++ ind 1 ++ r

instance ShowAscii SeesContext where
  showAscii (SeesContext c) = "\n" ++ ind 1 ++ c

instance ShowAscii Variable where
  showAscii (Variable v) = "\n" ++ ind 1 ++ printAscii v

instance ShowAscii Invariant where
  showAscii (Invariant lb pr) =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 2 else printAscii) pr

instance ShowAscii Variant where
  showAscii (Variant ex) =
      tail $ printAsciiLines 1 ex

instance ShowAscii RefinesEvent where
  showAscii (RefinesEvent ev) = ev

instance ShowAscii Parameter where
  showAscii (Parameter pa) = "\n" ++ ind 2 ++ printAscii pa

instance ShowAscii Guard where
  showAscii (Guard lb pr) =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 3 else printAscii) pr

instance ShowAscii Witness where
  showAscii (Witness lb pr) =
      "\n" ++ ind 2 ++ printAscii lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 3 else printAscii) pr

instance ShowAscii Action where
  showAscii (Action lb pr) =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 3 else printAscii) pr

instance ShowAscii ConvergenceType where
  showAscii Ordinary = ""
  showAscii Convergent = " convergent"
  showAscii Anticipated = " anticipated"

instance ShowAscii Event where
  showAscii (Event lb co _ re pa gu wi ac) =
      "\n" ++ ind 1 ++ lb ++ showAscii co ++
      (if not $ null re then "\n" ++ ind 1 ++ "REFINES " ++ (asciilist "," re) else "") ++
      (if not $ null pa then "\n" ++ ind 1 ++ "ANY"      ++ (asciilist ""  pa) else "") ++
      (if not $ null gu then "\n" ++ ind 1 ++ "WHERE"    ++ (asciilist ""  gu) else "") ++
      (if not $ null wi then "\n" ++ ind 1 ++ "WITH"     ++ (asciilist ""  wi) else "") ++
      (if not $ null ac then "\n" ++ ind 1 ++ "THEN"     ++ (asciilist ""  ac) else "") ++
      "\n" ++ ind 1 ++ "END\n"

instance ShowAscii Machine where
  showAscii (Machine na re se va inv var ev) =
      "MACHINE\n" ++ ind 1 ++ na ++
      (if not $ null re  then "\n" ++ "REFINES"    ++ (asciilist "" re ) else "") ++
      (if not $ null se  then "\n" ++ "SEES"       ++ (asciilist "" se ) else "") ++
      (if not $ null va  then "\n" ++ "VARIABLES"  ++ (asciilist "" va ) else "") ++
      (if not $ null inv then "\n" ++ "INVARIANTS" ++ (asciilist "" inv) else "") ++
      (if not $ null var then "\n" ++ "VARIANT"    ++ (asciilist "" var) else "") ++
      (if not $ null ev  then "\n" ++ "EVENTS"     ++ (asciilist "" ev ) else "") ++
      "\nEND"



