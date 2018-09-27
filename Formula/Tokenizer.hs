module Formula.Tokenizer (
    tokenize,
    reduce,
    tkn
    ) where

import Formula
import Formula.UTF8
import Data.Maybe
import Data.List (intercalate)
import Data.Either (fromRight)
import qualified Data.Set as S
import qualified Data.Char as C


isE :: (Enum a, ShowUTF8 a) => a -> String -> Bool
isE a s = s `elem` (map showUTF8 $ enumFrom a)

parseE :: (Enum a, ShowUTF8 a) => a -> String -> a
parseE a s = fromJust $ lookup s $ map (\x -> (showUTF8 x,x)) $ enumFrom a

isOperator :: String -> Bool
isOperator = isE Top

parseOperator :: String -> Operator
parseOperator = parseE Top

isSpecialIdent :: String -> Bool
isSpecialIdent = isE Integers

parseSpecialIdent :: String -> SpecialIdent
parseSpecialIdent = parseE Integers

isSpace :: String -> Bool
isSpace = isE SimpleSpace

parseSpace :: String -> Space
parseSpace = parseE SimpleSpace

isSimpleToken :: String -> Bool
isSimpleToken = isE TokOpenPar

parseSimpleToken :: String -> SimpleToken
parseSimpleToken = parseE TokOpenPar

defd :: Token -> Bool
defd TokUndef = False
defd _        = True

isident :: Token -> Bool
isident (TokIdent _) = True
isident (TokOpIdent _) = True
isident _ = False

isSpaceTk :: Token -> Bool
isSpaceTk (TokSpace _) = True
isSpaceTk _ = False

isIdentifierChar :: Char -> Bool
isIdentifierChar c = C.isAlphaNum c || c == '_' || c == '\''

reduce :: Either String [Token] -> [Token]
reduce = filter (not . isSpaceTk) . fromRight []

tokenize :: S.Set String -> String -> Either String [Token]
tokenize special str =
    spec <$> (tokenize_ TokUndef [] $ replaceEntities str)
    where spec = map change
          change x | TokIdent id <- x, id `elem` special = TokOpIdent id
                   | otherwise = x

tokenize_ :: Token -> String -> String -> Either String [Token]
tokenize_ current [] [] = if defd current then Right [current] else Right []
tokenize_ current [] tl = tokenize_ current [head tl] (tail tl)
tokenize_ current hd tl
    | isSpace hd =
        nextToken (TokSpace $ parseSpace hd) tl
    | isSimpleToken hd =
        nextToken (TokToken $ parseSimpleToken hd) tl
    | isOperator hd =
        nextToken (TokOp $ parseOperator hd) tl
    | not (null tl) && isOperator (hd ++ [head tl]) =
        nextToken (TokOp $ parseOperator (hd ++ [head tl])) (tail tl)
    | isSpecialIdent hd =
        nextToken (TokSpecialIdent $ parseSpecialIdent hd) tl
    | (not $ null hd) && isIdentifierChar (last hd) =
        case current of
          TokIdent id              -> tokenize_ (TokIdent (id ++ [last hd])) [] tl
          TokSpecialIdent Naturals -> if (last hd) == '1' then tokenize_ (TokSpecialIdent Naturals1) [] tl
                                                          else nextToken (TokIdent hd) tl
          TokOp Powerset           -> if (last hd) == '1' then tokenize_ (TokOp Powerset1) [] tl
                                                          else nextToken (TokIdent hd) tl
          _                        -> nextToken (TokIdent hd) tl
    | otherwise =
        if null tl
            then nextToken (TokIdent hd) tl
            else tokenize_ current (hd ++ [head tl]) (tail tl)
    where nextToken newcurrent tl' =
            let nxt = tokenize_ newcurrent [] tl' in
              if defd current
                  then (:) <$> Right current <*> nxt
                  else nxt

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

tkn :: String -> Formula
tkn = fromRight [] . tokenize S.empty




