module Substitution.Automata.Parser(
        AutomatonParseError,
        Pattern(..),
        ape_fragment, ape_position, ape_message,
        compile
    ) where

import Substitution.Automata
import Substitution.Automata.Label
import Substitution.Automata.Builder
import Substitution.Automata.Tokenizer
import Control.Applicative (liftA)
import Data.Char (isAlphaNum)

data Pattern =
      EmptyPattern
    | Capture  Pattern
    | Repeat   Pattern
    | Optional Pattern
    | Or       Pattern Pattern
    | Seq      Pattern Pattern
    | Lab Label

instance Show Pattern where
  show EmptyPattern = "{}"
  show (Capture p)  = "(" ++ show p ++ ")"
  show (Repeat p)   = show p ++ "*"
  show (Optional p) = show p ++ "?"
  show (Or p1 p2)   = show p1 ++ "|" ++ show p2
  show (Seq p1 p2)  = show p1 ++ show p2
  show (Lab la)     = show la

data AutomatonParseError = AutomatonParseError {
    fragment :: PTk,
    message  :: String
}

ape_position :: AutomatonParseError -> Int
ape_position = pos . fragment

ape_fragment :: AutomatonParseError -> String
ape_fragment = theToken . fragment

ape_message :: AutomatonParseError -> String
ape_message = message

instance Show AutomatonParseError where
  show pe =
      "Error:" ++ show (ape_position pe) ++ ": at '" ++ (ape_fragment pe) ++ "', " ++ (ape_message pe)

guessLabel :: String -> Maybe Label
guessLabel ""    = Nothing
guessLabel "\\o" = Just AnyOperator
guessLabel "\\I" = Just AnySpecialIdent
guessLabel "\\i" = Just AnyIdent
guessLabel "\\s" = Just AnySpace
guessLabel "\\k" = Just AnySimpleToken
guessLabel ('\\':c:_)
    | c `elem` ".*+|()!?<>\\\"" = Just $ PrintsLike [c]
    | otherwise = Nothing
guessLabel ('!':xs) = No <$> guessLabel xs
guessLabel str   = Just $ PrintsLike str

matchingCstr :: Pattern -> Pattern -> Bool
matchingCstr EmptyPattern EmptyPattern = True
matchingCstr (Capture _)  (Capture _)  = True
matchingCstr (Repeat _)   (Repeat _)   = True
matchingCstr (Optional _) (Optional _) = True
matchingCstr (Or _ _)     (Or _ _)     = True
matchingCstr (Seq _ _)    (Seq _ _)    = True
matchingCstr (Lab _)      (Lab _)      = True
matchingCstr _            _            = False

glob :: (Pattern -> Pattern) -> Pattern -> Pattern
glob _ EmptyPattern = EmptyPattern
glob f (Or  p1 p2) = Or  p1 $ glob f p2
glob f (Seq p1 p2) = Seq p1 $ glob f p2
glob f p
    | matchingCstr (f EmptyPattern) p = p
    | otherwise                       = f p

glob2 :: (Pattern -> Pattern -> Pattern) -> Pattern -> Pattern -> Pattern
glob2 _ EmptyPattern p            = p
glob2 _ p            EmptyPattern = p
glob2 f p1           p2           = f p1 p2


parse :: [PTk] -> Either AutomatonParseError Pattern
parse input =
    parse' (Right EmptyPattern) input
    where parse' :: Either AutomatonParseError Pattern -> [PTk] -> Either AutomatonParseError Pattern
          parse' last [] = last
          parse' last (x:xs) =
              case theToken x of
                "(" ->
                    case lookForClosing ("(",")") xs of
                      Nothing -> Left $ AutomatonParseError x "Unclosed parenthesis"
                      Just (captured,remainder) ->
                          let captpattern = Capture <$> parse' (Right EmptyPattern) captured in
                              parse' (glob2 Seq <$> last <*> captpattern) remainder
                ")" -> Left $ AutomatonParseError x "Extra closing parenthesis"
                "\""->
                    case lookForNext "\"" xs of
                      Nothing -> Left $ AutomatonParseError x "Unclosed double quote"
                      Just (quoted, remainder) ->
                          let merged = Right $ Lab $ PrintsLike $ foldl (\acc -> ((++) acc) . theToken) [] quoted in
                              parse' (glob2 Seq <$> last <*> merged) remainder
                "?" -> parse' (glob  Optional <$> last) xs
                "*" -> parse' (glob  Repeat   <$> last) xs
                "+" -> parse' (glob2 Seq      <$> last <*> (glob Repeat <$> last)) xs
                "|" ->
                    let remainder = parse' (Right EmptyPattern) xs in
                        glob2 Or <$> last <*> remainder
                "." -> parse' (glob2 Seq <$> last <*> (Right $ Lab AnyToken)) xs
                _  ->
                    case guessLabel $ theToken x of
                      Nothing -> Left $ AutomatonParseError x "Unexpected pattern element"
                      Just la -> parse' (glob2 Seq <$> last <*> (Right $ Lab la)) xs


normalize :: Pattern -> Pattern
normalize (Capture  (Capture  pt)) = Capture  pt
normalize (Repeat   (Repeat   pt)) = Repeat   pt
normalize (Optional (Optional pt)) = Optional pt
normalize (Repeat   (Capture  pt)) = Repeat   pt
normalize (Optional (Capture  pt)) = Capture (Optional pt)
normalize (Or EmptyPattern pt2 )   = pt2
normalize (Or pt1 EmptyPattern )   = pt1
normalize (Seq EmptyPattern pt2)   = pt2
normalize (Seq pt1 EmptyPattern)   = pt1
normalize x = x

derive_ :: Pattern -> Place -> Automaton -> AutoB Place
derive_ EmptyPattern = \pl -> \aut -> pl |>> aut >:> setValidating False
    >:> edgeto Always trap >:> getTo
derive_ (Capture pt) = \pl -> \aut -> pl |>> aut >:> setValidating False
    >:> edge BeginCapture >:> getTo >:> derive_ pt >:> setValidating False >:> edge EndCapture >:> getTo >:> setValidating True
derive_ (Repeat pt) = \pl -> \aut -> pl |>> aut
    >:> derive_ pt >:> setValidating False >:> edgeto Skip pl >:> getTo >:> setValidating True
derive_ (Optional pt) = \pl -> \aut -> pl |>> aut >:> setValidating False
    >:> buildsplit (>:> derive_ pt) (newEdge Skip) >:> getTo >:> setValidating True
derive_ (Or pt1 pt2) = \pl -> \aut -> pl |>> aut >:> setValidating False
    >:> buildcombine (\ab -> ab >:> derive_ pt1 >:> setValidating False)
                     (\ab -> ab >:> derive_ pt2 >:> setValidating False)
                     (const $ newEdge Skip)
    >:> getTo >:> setValidating True
derive_ (Seq pt1 pt2) = \pl -> \aut -> pl |>> aut >:> setValidating False
    >:> derive_ pt1 >:> derive_ pt2
derive_ (Lab la) = \pl -> \aut -> pl |>> aut >:> setValidating False
    >:> edge la >:> getTo >:> setValidating True

derive :: Pattern -> Automaton
derive patt =
    getAutomaton $ () |>> emptyAutomaton >>> newPlace >:> setinit >:> derive_ patt

compile :: String -> Either AutomatonParseError (Pattern,Automaton)
compile input = do
    patt <- parse $ tokenize $ input
    return (patt, derive $ normalize $ patt)



