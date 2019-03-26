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
import Data.Char (isSpace)

class ShowTeX a where
  showTeX :: a -> String

escape :: String -> String
escape = circle "(*" "*)"

math :: String -> String
math = circle "$" "$"

mathspace :: String -> String
mathspace input =
    let (spaces,remaining) = span isSpace input in
        spaces ++ (math remaining)

bold :: String -> String
bold = escape . circle "\\textbf{" "}"

italic :: String -> String
italic = escape . circle "\\textit{" "}"

texlist :: (ShowTeX a) => String -> [a] -> String
texlist = printlist showTeX

escape_ :: String -> String
escape_ =
    foldl (++) "" . map (\x -> if x `elem` needescape then ['\\',x] else [x])
    where needescape = "_\\{}[]^"



