module Substitution.Automata.Template.Parser(
        TemplateError,
        tpe_fragment, tpe_message,
        template',template
    ) where

import Substitution.Automata.Template
import Formula
import Formula.Tokenizer
import Ascii
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

template' :: [Token] -> Either TemplateError Template
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
                Nothing -> Left $ TemplateError ('$':tl) "cannot read group capture number"
                Just i  -> Right $ Rf i
          tpltk a = Right $ Tk a


template :: String -> Either TemplateError Template
template input =
    case tokenize showAscii input of
      Left pe -> Left $ TemplateError {
        tpe_fragment = fpe_fragment pe ++ " (" ++ (show $ fpe_position pe) ++ ")",
        tpe_message  = fpe_message pe
      }
      Right tks -> template' tks


