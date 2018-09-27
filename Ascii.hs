module Ascii where

import Util

class ShowAscii a where
  showAscii :: a -> String

asciilist :: (ShowAscii a) => String -> [a] -> String
asciilist = printlist showAscii


