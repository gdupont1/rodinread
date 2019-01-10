{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module TeX where

import Util

class ShowTeX a where
  showTeX :: a -> String

escape :: String -> String
escape = circle "(*" "*)"

math :: String -> String
math = circle "$" "$"

bold :: String -> String
bold = escape . circle "\\textbf{" "}"

italic :: String -> String
italic = escape . circle "\\textit{" "}"

texlist :: (ShowTeX a) => String -> [a] -> String
texlist = printlist showTeX

escape_ :: String -> String
escape_ = foldl (++) "" . map (\x -> if x == '_' then "\\_" else [x])



