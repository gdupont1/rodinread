{-|
Module      : Ascii
Description : Generic definition for writing ASCII
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com
-}
module Ascii where

import Util

-- | Class for representing a type that can be represented in ASCII
class ShowAscii a where
  -- | The only function to be defined is showAscii (that effectively transforms a object into ASCII)
  showAscii :: a -> String

-- | Helper function that applies showAscii to the given list and join the result with the given separator
asciilist :: (ShowAscii a) => String -> [a] -> String
asciilist = printlist showAscii


