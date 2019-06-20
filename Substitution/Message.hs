module Substitution.Message where

import Formula (Token)
import Ascii
import Substitution.Rule
import Wrap

data SubstitutionError = SubstitutionError {
    se_rule :: Rule,
    se_tokens :: [Token],
    se_message :: String
}

instance Show SubstitutionError where
  show se =
      "Error when executing rule "
      ++ (show $ se_rule se) ++ " on sequence '"
      ++ (foldl (++) "" $ map showAscii $ se_tokens se) ++ "': "
      ++ (se_message se)

data SubstitutionWarning = SubstitutionWarning {
    sw_rule :: Rule,
    sw_tokens :: [Token],
    sw_message :: String
}

instance Show SubstitutionWarning where
  show sw =
      "Warning when executing rule "
      ++ (show $ sw_rule sw) ++ " on sequence '"
      ++ (foldl (++) "" $ map showAscii $ sw_tokens sw) ++ "': "
      ++ (sw_message sw)


data SubstitutionTableError = SubstitutionTableError {
    ste_line :: Int,
    ste_fragment :: String,
    ste_message :: String
}

instance Show SubstitutionTableError where
  show ste =
      "Error when parsing substitution table: "
      ++ (if ste_line ste > 0 then "on line " ++ (show $ ste_line ste)
        ++ (if not $ null $ ste_fragment ste then ", at '" ++ (ste_fragment ste) ++ "'" else "")
        ++ ": " else "")
      ++ (ste_message ste)

data SubstitutionTableWarning = SubstitutionTableWarning {
    stw_line :: Int,
    stw_message :: String
}

instance Show SubstitutionTableWarning where
  show stw =
      "Warning: on line " ++ (show $ stw_line stw) ++ ": " ++ (stw_message stw)

type SWrap = Wrap SubstitutionError SubstitutionWarning
type SWrapT = WrapT SubstitutionError SubstitutionWarning

s_err :: (Rule,[Token]) -> String -> SubstitutionError
s_err (r,ts) m = SubstitutionError r ts m

s_warn :: (Rule,[Token]) -> String -> SubstitutionWarning
s_warn (r,ts) m = SubstitutionWarning r ts m

type STWrap = Wrap SubstitutionTableError SubstitutionTableWarning
type STWrapT = WrapT SubstitutionTableError SubstitutionTableWarning

st_err :: (Int,String) -> String -> SubstitutionTableError
st_err (l,f) m = SubstitutionTableError l f m

st_warn :: Int -> String -> SubstitutionTableWarning
st_warn = SubstitutionTableWarning




