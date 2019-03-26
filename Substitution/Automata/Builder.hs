module Substitution.Automata.Builder where

import qualified Data.Set as S
import Data.Maybe (fromJust)

import Substitution.Automata
import Substitution.Automata.Label

data AutoB a = AutoB Automaton a

instance Functor AutoB where
  fmap f (AutoB aut a) = AutoB aut $ f a

instance Show a => Show (AutoB a) where
  show (AutoB aut a) =
         "AutoB <<" ++ show a ++ ">>:\n"
      ++ " - Places: " ++ (show $ places aut) ++ "\n"
      ++ " - Edges : " ++ (show $ edges  aut) ++ "\n"

getAutomaton :: AutoB a -> Automaton
getAutomaton (AutoB aut _) = aut

get :: AutoB a -> a
get (AutoB _ a) = a

buildinit :: a -> Automaton -> AutoB a
buildinit a aut = AutoB aut a

start :: AutoB ()
start = AutoB emptyAutomaton ()

build0 :: AutoB a -> AutoB b -> AutoB b
build0 = const id

build1 :: AutoB a -> (Automaton -> AutoB b) -> AutoB b
build1 (AutoB aut _) f = f aut

build2 :: AutoB a -> (a -> Automaton -> AutoB b) -> AutoB b
build2 (AutoB aut a) f = f a aut

buildskip :: AutoB a -> (AutoB a -> AutoB b) -> AutoB a
buildskip ab@(AutoB aut a) f =
    let (AutoB aut' _) = f ab in
        AutoB aut' a

buildsplit :: (AutoB a -> AutoB b) -> (a -> b -> Automaton -> AutoB c) -> a -> Automaton -> AutoB c
buildsplit first second a aut =
    let (AutoB aut' b) = first $ AutoB aut a in
        second a b aut'

buildcombine :: (AutoB a -> AutoB b) -> (AutoB a -> AutoB c) -> (a -> b -> c -> Automaton -> AutoB d) -> a -> Automaton -> AutoB d
buildcombine first second combiner a aut =
    let (AutoB aut1 b) = first $ AutoB aut a in
        let (AutoB aut2 c) = second $ AutoB aut1 a in
            combiner a b c aut2

buildswap :: AutoB a -> b -> AutoB b
buildswap (AutoB aut _) = AutoB aut

acc0 :: AutoB a -> AutoB [a]
acc0 (AutoB aut a) = AutoB aut [a]

acc1 :: (Automaton -> AutoB a) -> (Automaton -> AutoB [a])
acc1 f = acc0 . f

acc2 :: (b -> Automaton -> AutoB a) -> (b -> Automaton -> AutoB [a])
acc2 f = acc1 . f

buildacc1 :: AutoB [a] -> (Automaton -> AutoB a) -> AutoB [a]
buildacc1 (AutoB aut as) f =
    let (AutoB aut' a) = f aut in
        AutoB aut' $ a:as

buildacc2 :: AutoB [a] -> (a -> Automaton -> AutoB a) -> AutoB [a]
buildacc2 (AutoB aut as@(a:_)) f =
    let (AutoB aut' a') = f a aut in
        AutoB aut' $ a':as

don1 :: Int -> (Automaton -> AutoB a) -> Automaton -> AutoB a
don1 n f aut =
    f aut >:> don (n-1) (const f)

don :: Int -> (a -> Automaton -> AutoB a) -> a -> Automaton -> AutoB a
don n f a aut =
    foldl (\autacc -> \_ -> autacc >:> f) (a |>> aut) [1..n]

donacc1 :: Int -> (Automaton -> AutoB a) -> Automaton -> AutoB [a]
donacc1 n f aut =
    f aut >:> donacc (n-1) (const f)

donacc :: Int -> (a -> Automaton -> AutoB a) -> a -> Automaton -> AutoB [a]
donacc n f a aut =
    foldl (\autacc -> \_ -> autacc +:> f) (acc0 $ a |>> aut) [1..n]



infix  8 |>>
infixl 1 >>>
infixl 1 >:>
infixl 1 ->>
infixl 1 #>>
infixl 1 #:>
infixl 1 +>>
infixl 1 +:>

(|>>) = buildinit
(>>>) = build1
(>:>) = build2
(->>) = buildskip
(#>>) = buildswap

(#:>) :: AutoB a -> (a -> b) -> AutoB b
(#:>) = flip fmap

(+>>) = buildacc1
(+:>) = buildacc2

setValidating :: Bool -> Place -> Automaton -> AutoB Place
setValidating val pl aut =
    AutoB (aut { validating = (if val then S.insert else S.delete) pl $ validating aut }) pl

newPlace :: Automaton -> AutoB Place
newPlace aut =
    let ct = counter aut in
        let pl = Place ct in
            AutoB (aut { counter = (ct + 1), places = S.insert pl $ places aut }) pl

place :: Place -> Automaton -> AutoB Place
place pl aut | S.member pl $ places aut =
    AutoB aut $ fromJust $ S.foldl isit Nothing $ places aut
    where isit j@(Just _) _   = j
          isit _          pl' | pl' == pl = Just pl'
                              | otherwise = Nothing

place' :: PlaceId -> Automaton -> AutoB Place
place' plid = place (Place plid)

newEdge :: Label -> Place -> Place -> Automaton -> AutoB Edge
newEdge la src dst aut =
    let ed = Edge { source = src, desti = dst, label = la } in
        AutoB (aut { edges = ed:(edges aut) }) ed

setinit :: Place -> Automaton -> AutoB Place
setinit pl aut =
    AutoB (aut { initState = pl }) pl

edge :: Label -> Place -> Automaton -> AutoB Edge
edge la src aut =
    let (AutoB aut' dest) = newPlace aut in
        newEdge la src dest aut'

edge' :: Label -> [Place] -> Automaton -> AutoB Edge
edge' la (dst:src:_) = newEdge la src dst

edgeto :: Label -> Place -> Place -> Automaton -> AutoB Edge
edgeto la dst src aut =
    newEdge la src dst aut

setdest :: (Automaton -> AutoB Place) -> Edge -> Automaton -> AutoB Edge
setdest getdest edge aut =
    let (AutoB aut' pl) = getdest aut in
        setdest' pl edge aut'

setdest' :: Place -> Edge -> Automaton -> AutoB Edge
setdest' newdest edge aut =
    let edge' = edge { desti = newdest } in
        AutoB (aut { edges = map (replaceIf edge edge') $ edges aut }) edge'
    where replaceIf mustmatch by edge = if edge == mustmatch then by else edge

self :: Label -> Place -> Automaton -> AutoB Edge
self la pl aut = newEdge la pl pl aut

-- from (place pl) to (place pl) via (lab)
-- from (newPlace) to (newPlace) via (lab)
type EdgePoint = (Automaton -> AutoB Place)

from :: EdgePoint -> (EdgePoint -> Automaton -> AutoB Place) -> EdgePoint -> (Label -> Automaton -> AutoB Label) -> Label -> Automaton -> AutoB Edge
from getsrc funto getdest getlab thelab aut =
    let (AutoB aut1 src) = getsrc aut in
        let (AutoB aut2 dst) = funto getdest aut1 in
            let (AutoB aut3 lab) = getlab thelab aut2 in
                newEdge lab src dst aut3

type ParamEdgePoint a = (a -> Automaton -> AutoB Place)

from' :: ParamEdgePoint a -> (ParamEdgePoint a -> a -> Automaton -> AutoB Place) -> ParamEdgePoint a -> (Label -> a -> Automaton -> AutoB Label) -> Label -> a -> Automaton -> AutoB Edge
from' getsrc funto getdest getlab thelab a aut =
    let (AutoB aut1 src) = getsrc a aut in
        let (AutoB aut2 dst) = funto getdest a aut1 in
            let (AutoB aut3 lab) = getlab thelab a aut2 in
                newEdge lab src dst aut3

to :: EdgePoint -> Automaton -> AutoB Place
to = ($)

to' :: ParamEdgePoint a -> a -> Automaton -> AutoB Place
to' = ($)

via :: Label -> Automaton -> AutoB Label
via = flip AutoB

via' :: Label -> a -> Automaton -> AutoB Label
via' la _ aut = AutoB aut la

getTo :: Edge -> Automaton -> AutoB Place
getTo edg aut = AutoB aut $ desti edg

getFrom :: Edge -> Automaton -> AutoB Place
getFrom edg aut = AutoB aut $ source edg

inverse :: Label -> Edge -> Automaton -> AutoB Edge
inverse la edge aut =
    newEdge la (desti edge) (source edge) aut

again :: Edge -> Automaton -> AutoB Edge
again edge aut =
    () |>> aut >>> newPlace >:> newEdge (label edge) (desti edge) 

nagain :: Int -> Edge -> Automaton -> AutoB Edge
nagain n =
    don n again

mergeplaces :: Place -> Place -> Automaton -> AutoB Place
mergeplaces pl1 pl2 aut =
    AutoB (aut {
        places    = S.delete pl2 $ places aut,
        edges     = map swapPlace $ edges aut,
        initState = if initState aut == pl2 then pl1 else initState aut
    }) pl1
    where swapPlace edge
              | source edge == pl2 && desti edge == pl2 = edge { source = pl1, desti = pl1 }
              | source edge == pl2 = edge { source = pl1 }
              | desti  edge == pl2 = edge { desti  = pl1 }
              | otherwise = edge

{-
newEdges_ fr las to aut =
    let (prefix,suffix) = splitLast las in
        let (aut',(edges,places,lastplace)) = newEdges_' fr prefix aut in
            let (aut'',lastedge) = newEdge_ lastplace suffix to aut' in
                (aut'',(lastedge:edges,places))
    where splitLast = (,) <$> init <*> last

newEdges_' :: [Label] -> Place -> Automaton -> (Automaton,(Edges,Places,Place))
newEdges_' las fr aut =
    foldl next (aut,([],S.empty,fr)) las
    where next (auto,(edges,places,lastplace)) la =
            let (auto',(edge,place)) = newEdge_' la lastplace auto in
                (auto',(edge:edges,S.insert place places,place))

newSeq_' :: Place -> [Label] -> Automaton -> (Automaton,(Edges,Places,Place))
newSeq_' fr las aut =
    foldl next (aut,([],S.empty,fr)) las
    where next (auto,(edges,places,lastplace)) la =
            let (auto',(edge,place)) = newEdge_' lastplace la auto in
                (auto',(edge:edges,S.insert place places,place))

anytime_ :: Place -> Label -> Automaton -> (Automaton,Edge)
anytime_ pl la aut =
    pl |>> aut >:> setPlaceValidating_ True >>> selfEdge_ pl la

-}
