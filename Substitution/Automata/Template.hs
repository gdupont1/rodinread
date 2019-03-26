module Substitution.Automata.Template(
        TemplateError(..),
        TemplateToken(..), Template,
        fill
    ) where

import Formula
import Formula.Ascii
import Ascii
import Control.Applicative (liftA2)

data TemplateError = TemplateError {
    tpe_fragment :: String,
    tpe_message :: String
}

instance Show TemplateError where
  show tp =
      "Error: at '" ++ tpe_fragment tp ++ "': " ++ tpe_message tp

data TemplateToken = 
      Tk Token
    | Rf Int
    deriving (Eq)

instance Show TemplateToken where
  show (Tk tk) = showAscii tk
  show (Rf i)  = "$" ++ show i


type Template = [TemplateToken]

fill :: [Token] -> [[Token]] -> Template -> Either TemplateError [Token]
fill matched captures temp =
    foldl (liftA2 (++)) (Right []) $ map parse1 temp
    where parse1 (Tk tk) = Right [tk]
          parse1 r@(Rf i )
              | i > length captures || i < 0 = Left $ TemplateError (show r) $ "capture number invalid. Valid range for this pattern is [0.." ++ (show $ length captures) ++ "]."
              | i == 0 = Right matched
              | otherwise = Right $ captures !! (i - 1)



