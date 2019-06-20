module Substitution.Automata.Reader(
        AutomatonReader,
        captures,readTokens,remainingTokens,
        reader,validatingState,trapped,
        finished,finishedorvalidating,
        step,stepall,stepuntilvalidating
    )where

import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Set as S

import Substitution.Automata
import Substitution.Automata.Label
import Formula

data ATk = AT Token | AEnd

instance Show ATk where
  show (AT t) = show t
  show AEnd = "$$$"

remainingTokens :: AutomatonReader -> [Token]
remainingTokens autrd =
    map ext $ takeWhile notend $ remainingATokens autrd
    where notend AEnd = False
          notend _    = True
          ext (AT tk) = tk

data AutomatonReader = AutomatonReader {
    automaton       :: Automaton,
    currentState    :: Place,
    currentCaptures :: [[Token]],
    captures        :: [[Token]],
    readTokens      :: [Token],
    remainingATokens :: [ATk],
    edgesTook       :: [Edge]
} deriving Show

reader :: Automaton -> [Token] -> AutomatonReader
reader aut tks =
    skipall $ AutomatonReader {
        automaton = aut,
        currentState = initState aut,
        currentCaptures = [],
        captures = [],
        readTokens = [],
        remainingATokens = (map AT tks) ++ [AEnd],
        edgesTook = []
    }

validatingState :: AutomatonReader -> Bool
validatingState rd = S.member (currentState rd) (validating $ automaton $ rd)

trapped :: AutomatonReader -> Bool
trapped = (< 0) . ident . currentState

nextedges' :: Automaton -> Place -> [[Edge]]
nextedges' aut pl =
    sortBy (compare `on` (label . last)) $ foldl (++) [] $ map resolve $ simplenext pl
    where simplenext pl = filter ((== pl) . source) $ edges aut
          resolve edge
              | isConsuming $ label edge = [[edge]]
              | otherwise = map (edge:) $ nextedges' aut $ desti edge

nextedges :: AutomatonReader -> [[Edge]]
nextedges autrd = nextedges' (automaton autrd) (currentState autrd)

findfirst :: (a -> Bool) -> [a] -> Maybe a
findfirst _ [] = Nothing
findfirst p (t:ts)
    | p t = Just t
    | otherwise = findfirst p ts

nextedge :: AutomatonReader -> Token -> Maybe [Edge]
nextedge autrd tk = findfirst ((//> tk) . label . last) $ nextedges autrd

skipall :: AutomatonReader -> AutomatonReader
skipall autrd =
    case nextskip autrd of
      Nothing -> autrd
      Just e  -> skipall $ rawstep autrd e --(newcapturemode autrd e) e 
      where nextskip autrd = findfirst (isSkip . label) $ filter ((== (currentState autrd)) . source) $ edges $ automaton autrd

rawstep :: AutomatonReader -> Edge -> AutomatonReader
rawstep autrd edge = autrd { currentState = desti edge, edgesTook = (edgesTook autrd) ++ [edge] }

step :: AutomatonReader -> AutomatonReader
step autrd =
    case remainingATokens autrd of
      []            -> autrd
      (AEnd:_)      -> wrapup $ autrd { remainingATokens = [] }
      ((AT tk):tks) ->
          case nextedge autrd tk of
          Nothing ->
              autrd { currentState = trap 
                    , readTokens = tk:(readTokens autrd)
                    , remainingATokens = tks }
          Just edges ->
              let autrd' = foldl onestep autrd $ init edges
                  edge   = last edges in
                  let autrd'' = newcapturemode autrd' edge in
                      let (newreadTokens,newremainingTokens,newcaptures) = consume edge tk tks (readTokens autrd'') (currentCaptures autrd'') in
                          skipall $ ((rawstep autrd'' edge) {
                            currentCaptures = newcaptures
                            , readTokens = newreadTokens
                            , remainingATokens = newremainingTokens
                          })
    where consume edge tk tks rtks ctks
            | isConsuming $ label edge = (tk:rtks,tks,map ((:) tk) ctks)
            | otherwise                = (rtks,(AT tk):tks,ctks)
          wrapup autrd =
              if validatingState autrd then autrd
              else
                case nextendcapture autrd of
                  Nothing -> autrd
                  Just e -> wrapup $ skipall $ rawstep (newcapturemode autrd e) e
          nextendcapture autrd =
              findfirst ((== EndCapture) . label) $ filter ((== currentState autrd) . source) $ edges $ automaton autrd
          onestep autrd edge = skipall $ rawstep (newcapturemode autrd edge) edge

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

finished :: AutomatonReader -> Bool
finished rd = (null $ remainingATokens rd) || (trapped rd)

finishedorvalidating :: AutomatonReader -> Bool
finishedorvalidating rd = (validatingState rd) || (finished rd)

stepall :: AutomatonReader -> AutomatonReader
stepall =
    head . dropWhile (not . finished) . iterate step

stepuntilvalidating :: AutomatonReader -> AutomatonReader
stepuntilvalidating =
    head . dropWhile (not . finishedorvalidating) . iterate step


