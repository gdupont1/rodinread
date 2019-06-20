{-|
Module      : Formula.Util
Description : utility for dealing with formulas
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

Module with utility functions for dealing with formulas.
-}
module Formula.Util where

import Util
import Formula
--import Formula.Tokenizer
import Data.List.Split (splitWhen)


isMath :: Token -> Bool
isMath (TokOp _) = True
isMath (TokSpecialIdent _) = True
isMath (TokToken TokDot) = True
isMath (TokToken TokMid) = True
isMath _ = False

isSpace :: Token -> Bool
isSpace (TokSpace _) = True
isSpace _ = False

functionLike :: Operator -> Bool
functionLike =
    (flip elem) [
      Not
    , Powerset
    , Powerset1
    , GenUnion
    , GenIntersection
    , Inverse
    , Lambda
    ]


isNewline :: Token -> Bool
isNewline (TokSpace Newline) = True
isNewline _ = False

printLines' :: ([Token] -> String) -> [Token] -> [String]
printLines' printLine tks =
    let tkss = splitWhen isNewline tks in
        map printLine tkss

printLines :: ([Token] -> String) -> Int -> [Token] -> String
printLines printLine indent =
    foldl (\acc -> (++) $ acc ++ "\n" ++ ind indent) "" . (printLines' printLine)

pruneType :: ([Token],[Token]) -> ([Token],[Token])
pruneType (acc,[]) = (acc,[])
pruneType (acc,(x:xs)) =
    case x of
      TokSpace _             -> pruneType (acc++[x],xs)
      TokOp Powerset         -> pruneType (acc++[x],xs)
      TokToken TokOpenPar    -> pruneType $ takeOutParentheses (acc++[x],xs)
      TokIdent _             -> pruneType (acc++[x],xs)
      TokOp CartesianProduct -> pruneType (acc++[x],xs)
      _                      -> (acc,x:xs)

takeOutParentheses :: ([Token],[Token]) -> ([Token],[Token])
takeOutParentheses (acc,[]) = (acc,[])
takeOutParentheses (acc,(x:xs)) =
    case x of
      TokToken TokOpenPar  -> let (acc',xs') = takeOutParentheses ([x],xs) in takeOutParentheses (acc ++ acc', xs')
      TokToken TokClosePar -> (acc ++ [x], xs)
      _                    -> takeOutParentheses (acc ++ [x], xs)


untype :: [Token] -> [Token]
untype [] = []
untype ((TokSpace _):(TokOp OfType):xs) = untype ((TokOp OfType):xs)
untype ((TokOp OfType):xs) =
    let (acc,xs') = pruneType ([],xs)
      in untype xs'
untype (x:xs) = x:(untype xs)

removeAloneParentheses :: [Token] -> [Token]
removeAloneParentheses tks =
    aux Nothing tks
    where aux _ [] = []
          aux Nothing ((TokToken TokOpenPar):i:(TokToken TokClosePar):xs) = i:(aux Nothing xs)
          aux (Just (TokOp o)) l@((TokToken TokOpenPar):i:(TokToken TokClosePar):xs)
              | not $ functionLike o = i:(aux Nothing xs)
              | otherwise = (head l):(aux (Just $ head l) (tail l))
          aux (Just (TokSpace _)) l@((TokToken TokOpenPar):i:(TokToken TokClosePar):xs) =
              i:(aux Nothing xs)
          aux (Just (TokToken x)) l@((TokToken TokOpenPar):i:(TokToken TokClosePar):xs)
              | x /= TokClosePar = i:(aux Nothing xs)
              | otherwise = (head l):(aux (Just $ head l) (tail l))
          aux _ (x:xs) = x:(aux (Just x) xs)




