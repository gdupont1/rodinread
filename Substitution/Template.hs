module Substitution.Template(
        TemplateError(..), TemplateWarning, TWrap,
        TemplateToken(..), Template,
        fill
    ) where

import Formula
import Formula.Ascii
import Ascii
import Wrap
import Control.Monad (foldM)

data TemplateError = TemplateError {
    tpe_fragment :: String,
    tpe_message :: String
}

instance Show TemplateError where
  show tp =
      "Error: at '" ++ tpe_fragment tp ++ "': " ++ tpe_message tp

data TemplateWarning

instance Show TemplateWarning where
  show _ = ""

type TWrap = Wrap TemplateError TemplateWarning

data TemplateToken = 
      Tk Token
    | Rf Int
    deriving (Eq)

instance Show TemplateToken where
  show (Tk tk) = showAscii tk
  show (Rf i)  = "$" ++ show i


type Template = [TemplateToken]

fill :: [Token] -> [[Token]] -> Template -> TWrap [Token]
fill matched captures temp =
    foldM parse1 [] temp
    where parse1 :: [Token] -> TemplateToken -> TWrap [Token]
          parse1 acc (Tk tk) = return $ acc ++ [tk]
          parse1 acc r@(Rf i )
              | i > length captures || i < 0 = failwith' TemplateError (show r) $ "capture number invalid. Valid range for this pattern is [0.." ++ (show $ length captures) ++ "]."
              | i == 0 = return $ acc ++ matched
              | otherwise = return $ acc ++ (captures !! (i - 1))



