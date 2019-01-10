{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module RodinContext.Ascii where

import Util
import Ascii
import RodinContext
import Formula
import Formula.Util
import Formula.Ascii

instance ShowAscii ExtendsContext where
  showAscii (ExtendsContext tg) = "\n" ++ ind 1 ++ tg

instance ShowAscii Axiom where
  showAscii (Axiom lb pr _) =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 2 else printAscii) pr

instance ShowAscii Constant where
  showAscii (Constant nm) =
      "\n" ++ ind 1 ++ nm 

instance ShowAscii CarrierSet where
  showAscii (CarrierSet nm) =
      "\n" ++ ind 1 ++ nm

instance ShowAscii Context where
  showAscii (Context nm ec ax ct cs) =
      "CONTEXT\n" ++ ind 1 ++ nm ++
      (if not $ null ec then "\n" ++ "EXTENDS"   ++ (asciilist "" ec) else "") ++
      (if not $ null cs then "\n" ++ "SETS"      ++ (asciilist "" cs) else "") ++
      (if not $ null ct then "\n" ++ "CONSTANTS" ++ (asciilist "" ct) else "") ++
      (if not $ null ax then "\n" ++ "AXIOMS"    ++ (asciilist "" ax) else "") ++
      "\nEND"



