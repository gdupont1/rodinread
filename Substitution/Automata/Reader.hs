module Substitution.Automata.Reader(
        AutomatonReader,
        captures,readTokens,remainingTokens,
        reader,validatingState,trapped,
        step,stepall,stepuntilvalidating
    )where

import Data.List (find)
import qualified Data.Set as S

import Substitution.Automata
import Substitution.Automata.Label
import Formula

data AutomatonReader = AutomatonReader {
    automaton       :: Automaton,
    currentState    :: Place,
    currentCaptures :: [[Token]],
    captures        :: [[Token]],
    readTokens      :: [Token],
    remainingTokens :: [Token]
} deriving Show

reader :: Automaton -> [Token] -> AutomatonReader
reader aut tks =
    AutomatonReader {
        automaton = aut,
        currentState = initState aut,
        currentCaptures = [],
        captures = [],
        readTokens = [],
        remainingTokens = tks
    }

validatingState :: AutomatonReader -> Bool
validatingState rd = S.member (currentState rd) (validating $ automaton $ rd)

trapped :: AutomatonReader -> Bool
trapped = (< 0) . ident . currentState

step :: AutomatonReader -> AutomatonReader
step autrd =
    case remainingTokens autrd of
      [] -> autrd
      (tk:tks) ->
        case find ((//> tk) . label) nextedges of
          Nothing ->
              autrd { currentState = trap 
                    , readTokens = tk:(readTokens autrd)
                    , remainingTokens = tks }
          Just edge ->
              let autrd' = newcapturemode autrd edge in
                  let newstate = desti edge
                      (newreadTokens,newremainingTokens,newcaptures) = consume edge tk tks (readTokens autrd') (currentCaptures autrd') in
                      autrd' { currentState = newstate
                             , currentCaptures = newcaptures
                             , readTokens = newreadTokens
                             , remainingTokens = newremainingTokens }
    where nextedges = filter ((==) (currentState autrd) . source) $ edges $ automaton autrd 
          consume edge tk tks rtks ctks
            | isConsuming $ label edge = (tk:rtks,tks,map ((:) tk) ctks)
            | otherwise                = (rtks,tk:tks,ctks)

newcapturemode :: AutomatonReader -> Edge -> AutomatonReader
newcapturemode autrd edge =
    case capturemode edge of
      NoCapture -> autrd
      BeginMode -> autrd { currentCaptures = []:(currentCaptures autrd) }
      EndMode   ->
          let (capture,remaining) = (,) <$> head <*> tail $ currentCaptures autrd in
              autrd {
                currentCaptures = remaining,
                captures = (captures autrd) ++ [reverse capture]
              }

stepall :: AutomatonReader -> AutomatonReader
stepall =
    head . dropWhile (not . finished) . iterate step
    where finished rd = (null $ remainingTokens rd) || (trapped rd)

stepuntilvalidating :: AutomatonReader -> AutomatonReader
stepuntilvalidating =
    head . dropWhile (not . finished) . iterate step
    where finished rd = (validatingState rd) || (null $ remainingTokens rd) || (trapped rd)


