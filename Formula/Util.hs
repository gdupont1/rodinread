module Formula.Util where

import Util
import Formula
import Formula.Tokenizer
import Data.List.Split (splitWhen)


isMath :: Token -> Bool
isMath (TokOp _) = True
isMath (TokSpecialIdent _) = True
isMath (TokToken TokDot) = True
isMath (TokToken TokMid) = True
isMath _ = False

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




