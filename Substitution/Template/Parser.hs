module Substitution.Template.Parser(
        template',template
    ) where

import Wrap
import Substitution.Template
import Formula
import Formula.Tokenizer
import Ascii
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

template' :: [Token] -> TWrap Template
template' tks =
    sequence $ map tpltk $ foldr normalize [] tks
    where normalize tk acc =
            case tk of
              TokIdent elt | '$' `elem` elt ->
                  let (hd:tl) = splitOn "$" elt in
                      let tl' = map ('$':) tl in
                          let ids' = map TokIdent $ filter (not . null) (hd:tl') in
                              ids' ++ acc
              _ -> tk:acc
          tpltk (TokIdent ('$':tl)) =
              case readMaybe tl of
                Nothing -> failwith' TemplateError ('$':tl) "cannot read group capture number"
                Just i  -> return $ Rf i
          tpltk a = return $ Tk a


template :: String -> TWrap Template
template input = do
    tks <- transpose fpe_to_te fpw_to_tw $ tokenize showAscii input
    template' tks
    where fpe_to_te pe = TemplateError (fpe_fragment pe ++ " (" ++ (show $ fpe_position pe) ++ ")") (fpe_message pe)
          fpw_to_tw _  = []


