{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module Util where

import Data.List (intercalate)

indent_ :: String -> Int -> String
indent_ _ 0 = ""
indent_ s n = s ++ indent_ s (n - 1)

ind :: Int -> String
ind = indent_ "  "

circle :: String -> String -> String -> String
circle before after = ((++) before) . (++ after)

printlist :: (a -> String) -> String -> [a] -> String
printlist printer c = intercalate c . map printer



