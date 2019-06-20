{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module Formula.Tokenizer (
    isIdentChar,
    tokenize,
    reduce,
    tkn,
    replaceEntities,
    FormulaParseError,FormulaParseWarning,
    fpe_position,fpe_fragment,fpe_message
    ) where

import Wrap
import Formula
import Formula.UTF8
import Formula.Ascii
import Data.List (groupBy)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.Char as C
import Data.Function (on)


isSpaceTk :: Token -> Bool
isSpaceTk (TokSpace _) = True
isSpaceTk _ = False

data Nature = Void | Number | Ident | Operator | Quote | Space deriving (Enum,Eq)

instance Show Nature where
  show Void = "V"
  show Number = "N"
  show Ident = "I"
  show Operator = "O"
  show Space = "S"
  show Quote = "Q"

isIdentChar :: Char -> Bool
isIdentChar c = C.isAlpha c || c == '_' || c == '\'' || c == '$'

nature :: Char -> Nature
nature c
    | isIdentChar c = Ident
    | C.isNumber c = Number
    | C.isSpace c = Space
    | c == '"' = Quote
    | otherwise = Operator

data FTk = FTk {
    ftkcontent  :: String,
    ftknature   :: Nature,
    ftkposition :: Int
}

infixr 5 +:

(+:) :: Char -> FTk -> FTk
c +: ftk = ftk { ftkcontent = c:(ftkcontent ftk) }

reversef :: FTk -> FTk
reversef ftk = ftk { ftkcontent = reverse (ftkcontent ftk) }

instance Show FTk where
  show (FTk ct na po) = show ct ++ "[[" ++ show na ++ "]]<<" ++ show po ++ ">>"

explode :: String -> [FTk]
explode input =
    let (ftks,curr) = foldl eat ([],FTk [] Void 0) $ zip input [1..] in
        reverse ((reversef curr):ftks)
    where eat :: ([FTk],FTk) -> (Char,Int) -> ([FTk],FTk)
          eat (acc,curr) (c,pos) =
              let nat = ftknature curr
                  nat' = nature c in
                  case nat of
                    Void ->
                        (acc,FTk [c] nat' pos)
                    Number ->
                        case nat' of
                          Number -> (acc,c+:curr)
                          _      -> ((reversef curr):acc,FTk [c] nat' pos)
                    Ident ->
                        case nat' of
                          Ident  -> (acc,c+:curr)
                          Number -> (acc,c+:curr)
                          _      -> ((reversef curr):acc,FTk [c] nat' pos)
                    Operator ->
                        ((reversef curr):acc,FTk [c] nat' pos)
                    Space ->
                        ((reversef curr):acc,FTk [c] nat' pos)
                    Quote ->
                        case nat' of
                          Quote ->
                              let escaping = takeWhile (== '\\') $ ftkcontent curr in
                                  if (length escaping) `mod` 2 == 1 then
                                    (acc,c+:curr)
                                  else
                                    ((reversef (c+:curr)):acc, FTk [] Void pos)
                          _     -> (acc,c+:curr)

data FormulaParseError = FormulaParseError {
    fragment :: FTk,
    message :: String
}

fpe_position :: FormulaParseError -> Int
fpe_position = ftkposition . fragment

fpe_fragment :: FormulaParseError -> String
fpe_fragment = ftkcontent . fragment

fpe_message :: FormulaParseError -> String
fpe_message = message

instance Show FormulaParseError where
  show pe =
      "Error:" ++ show (ftkposition $ fragment pe) ++ ": at '" ++ (ftkcontent $ fragment pe) ++ "', " ++ (message pe)

data FormulaParseWarning

type FPWrap = Wrap FormulaParseError FormulaParseWarning

reduce :: FPWrap [Token] -> [Token]
reduce = filter (not . isSpaceTk) . getContent []

extract :: (Token -> String) -> [FTk] -> FPWrap [Token]
extract printer tks =
    getTkns tks
    where getTkns :: [FTk] -> FPWrap [Token]
          getTkns [] = return []
          getTkns l@(tk:tks)
            | ftknature tk == Operator =
                let (op,rem) = span ((== Operator) . ftknature) l in
                    (++) <$> guessOps tk (reverse op) [] <*> getTkns rem
            | otherwise =
                (++) <$> getOneTk tk <*> getTkns tks
          getOneTk :: FTk -> FPWrap [Token]
          getOneTk f@(FTk ct na po) 
            | na == Number   = return $ [TokIdent ct]
            | na == Space    = return $ [parseTk printer TokSpace SimpleSpace ct]
            | na == Ident    = return $ [search ct]
            | na == Quote    =
                if last ct /= '"' then
                  failwith' FormulaParseError f $ "Unclosed quote"
                else
                  let ct' = init $ tail $ ct in return [search ct']
            | na == Operator =
                case search ct of
                  TokIdent _ -> failwith' FormulaParseError f $ "Unexpected operator '" ++ ct ++ "'"
                  tk         -> return [tk]
            | na == Void = return []
          search :: String -> Token
          search tk
            | isTk printer TokOp           Top         tk = parseTk printer TokOp           Top         tk
            | isTk printer TokSpace        SimpleSpace tk = parseTk printer TokSpace        SimpleSpace tk
            | isTk printer TokSpecialIdent Integers    tk = parseTk printer TokSpecialIdent Integers    tk
            | isTk printer TokToken        TokOpenPar  tk = parseTk printer TokToken        TokOpenPar  tk
            | otherwise = TokIdent tk
          --          Ctxt   Consid.  Remainder
          guessOps :: FTk -> [FTk] -> [FTk] -> FPWrap [Token]
          guessOps ctx [] []  = return []
          guessOps ctx [] rem = failwith' FormulaParseError ctx $ "Unexpected operator(s)"
          guessOps ctx cs rem =
              let red = reduction $ reverse cs in
                case search $ ftkcontent red of
                  TokIdent id ->
                      guessOps ctx (tail cs) ((head cs):rem)
                  tk ->
                      let mop = guessOps ctx (reverse rem) [] in
                          if isError mop then guessOps ctx ((head rem):cs) (tail rem)
                          else (tk:) <$> mop
          reduction :: [FTk] -> FTk
          reduction r = (head r) { ftkcontent = foldl (\acc -> \x -> acc ++ (ftkcontent x)) "" r }
          isTk :: (Enum a) => (Token -> String) -> (a -> Token) -> a -> String -> Bool
          isTk printer fun elt tk = tk `elem` (map (printer.fun) $ enumFrom elt)
          parseTk :: (Enum a) => (Token -> String) -> (a -> Token) -> a -> String -> Token
          parseTk printer fun elt tk = fun $ fromJust $ lookup tk $ map (\x -> (printer $ fun x,x)) $ enumFrom elt


replaceEntities :: String -> String
replaceEntities [] = []
replaceEntities s@(x:xs)
    | x == '&' =
        let (ent,rem) = span (/= ';') xs in
            if null rem
                then s
                else (replaceOne ent) ++ (replaceEntities (tail rem))
    | otherwise = x:(replaceEntities xs)
    where replaceOne "lt" = "<"
          replaceOne "gt" = ">"
          replaceOne "amp" = "&"
          replaceOne ('#':xs) = [C.chr $ read xs]

tokenize :: (Token -> String) -> String -> FPWrap Formula
tokenize printer = extract printer . explode

tkn :: String -> Formula
tkn = getContent [] . tokenize showUTF8 . replaceEntities




