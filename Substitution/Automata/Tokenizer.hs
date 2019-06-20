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

cat :: PTk -> [PTk] -> [PTk]
cat (PTk [] _)  current = current
cat new         current = new:current

tokenize' :: (PTk,[PTk]) -> Int -> String -> [PTk]
tokenize' (rcurrent,rtokens) pos [] = (reversep rcurrent) `cat` rtokens
tokenize' (rcurrent,rtokens) pos (x:xs)
    | checkhead rcurrent '\\' =
        tokenize' (PTk [] (pos+1),reversep (x+:rcurrent) `cat` rtokens) (pos+1) xs
    | isIdentChar x =
        tokenize' (x +: rcurrent,rtokens) (pos+1) xs
    | x == '\\' =
        tokenize' (PTk [x] pos,reversep rcurrent `cat` rtokens) (pos+1) xs
    | x == '"' =
        let (pos',quote,rem) = eatquote pos "" xs in
            tokenize' (PTk [] (pos'+1),PTk quote pos `cat` (reversep rcurrent `cat` rtokens)) (pos'+1) rem
    | otherwise =
        tokenize' (PTk [] (pos+1),(PTk [x] pos):(reversep rcurrent `cat` rtokens)) (pos+1) xs
    where checkhead (PTk []     _) c = False
          checkhead (PTk (x:xs) _) c = x == c
          reversep (PTk tk pos) = PTk (reverse tk) pos
          isIdentChar c = c == '_' || c == '\'' || isAlphaNum c

eatquote :: Int -> String -> String -> (Int,String,String)
eatquote n acc [] = (n,reverse acc,[])
eatquote n acc ('\\':'"':xs) = eatquote (n+2) ('"':'\\':acc) xs
eatquote n acc ('"':xs) = (n+1,reverse acc,xs)
eatquote n acc (x:xs) = eatquote (n+1) (x:acc) xs

tokenize :: String -> [PTk]
tokenize =
    reverse . tokenize' (PTk [] 0,[]) 0
    {-reverse . decapitate . foldl tkn_ [PTk [] 0] . zip [0..]
    where tkn_ (rcurrent:rtokens) (pos,x)
              | checkhead (theToken rcurrent) '\\' =
                  (PTk [] (pos+1)):(reversep (x+:rcurrent)) `cat` rtokens
              | isIdentChar x =
                  (x+:rcurrent) `cat` rtokens
              | x == '\\' =
                  (PTk [x] pos):((reversep rcurrent) `cat` rtokens)
              | otherwise =
                  (PTk [] (pos+1)):(PTk [x] pos):((reversep rcurrent) `cat` rtokens)
          isIdentChar = (||) <$> (== '_') <*> isAlphaNum
          decapitate ((PTk [] _):xs) = xs
          decapitate (x:xs)          = (reversep x):xs
          cat (PTk [] _)  current = current
          cat new         current = new:current
          reversep (PTk tk pos) = PTk (reverse tk) pos
          checkhead []     _= False
          checkhead (x:xs) c = x == c-}

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




