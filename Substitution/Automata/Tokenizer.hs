module Substitution.Automata.Tokenizer where

import Data.Char (isAlphaNum)

data PTk = PTk {
    theToken :: String,
    pos :: Int
}

instance Show PTk where
  show (PTk tk p) = show tk ++ "<<" ++ show p ++ ">>"

infixr 5 +:
infix 4 ===

(+:) :: Char -> PTk -> PTk
c +: (PTk tk pos) = PTk (c:tk) pos

(===) :: PTk -> String -> Bool
(PTk tk _) === str = tk == str

reversep :: PTk -> PTk
reversep (PTk tk pos) = PTk (reverse tk) pos

tokenize :: String -> [PTk]
tokenize =
    reverse . decapitate . foldl tkn_ [PTk [] 0] . zip [0..]
    where tkn_ (rcurrent:rtokens) (pos,x)
              | checkhead (theToken rcurrent) '\\' =
                  (PTk [] (pos+1)):(reversep (x+:rcurrent)) `cat` rtokens
              | isIdentChar x =
                  (x+:rcurrent) `cat` rtokens
              | x == '\\' && theToken rcurrent == "!" =
                  (x+:rcurrent) `cat`rtokens
              | x == '\\' =
                  (PTk [x] pos):((reversep rcurrent) `cat` rtokens)
              | x == '!' =
                  (PTk [x] pos):((reversep rcurrent) `cat` rtokens)
              | otherwise =
                  (PTk [] (pos+1)):(PTk [x] pos):((reversep rcurrent) `cat` rtokens)
          isIdentChar = (||) <$> (== '_') <*> isAlphaNum
          decapitate ((PTk [] _):xs) = xs
          decapitate (x:xs)                   = (reversep x):xs
          cat (PTk [] _)  current = current
          cat new         current = new:current
          reversep (PTk tk pos) = PTk (reverse tk) pos
          checkhead []     _= False
          checkhead (x:xs) c = x == c

lookForClosing :: (String,String) -> [PTk] -> Maybe ([PTk],[PTk])
lookForClosing conf =
    lookForClosing' conf [] 0
    where lookForClosing' :: (String,String) -> [PTk] -> Int -> [PTk] -> Maybe ([PTk],[PTk])
          lookForClosing' (opening,closing) acc 0 (x:rem)
              | x === closing = Just (reverse acc,rem)
          lookForClosing' (opening,closing) acc _ [] =
              Nothing
          lookForClosing' (opening,closing) acc n (x:rem)
              | x === closing = lookForClosing' (opening,closing) (x:acc) (n-1) rem
              | x === opening = lookForClosing' (opening,closing) (x:acc) (n+1) rem
              | otherwise     = lookForClosing' (opening,closing) (x:acc)   (n) rem

lookForNext :: String -> [PTk] -> Maybe ([PTk],[PTk])
lookForNext next =
    lookForNext' next []
    where lookForNext' :: String -> [PTk] -> [PTk] -> Maybe ([PTk],[PTk])
          lookForNext' next acc [] =
              Nothing
          lookForNext' next acc (x:rem) 
              | x === next = Just (reverse acc,rem)
              | otherwise  = lookForNext' next (x:acc) rem




