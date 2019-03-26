{-# LANGUAGE TupleSections #-}
module Substitution.Automata where

import Data.Function (on)
import qualified Data.Set as S
import Data.List (intersect)

import Formula

import Substitution.Automata.Label

data Edge = Edge {
    label  :: Label,
    source :: Place,
    desti  :: Place
} deriving (Eq)


instance Show Edge where
  show edg =
      (show $ source edg) ++ " --[" ++ (show $ label edg) ++ "]--> " ++ (show $ desti edg)

data CaptureMode = NoCapture | BeginMode | EndMode deriving (Eq,Ord,Enum,Show)

capturemode :: Edge -> CaptureMode
capturemode edg =
    case label edg of
      BeginCapture -> BeginMode
      EndCapture   -> EndMode
      _            -> NoCapture

type PlaceId = Int
data Place = Place {
    ident       :: PlaceId--,
--    validating  :: Bool
} deriving (Eq)

trap :: Place
trap = Place (-1)


instance Ord Place where
  compare = compare `on` ident

instance Show Place where
  show pl =
      "(" ++ placename ++ ")"
      where placename =
                if pl == trap then "X" else show $ ident pl

type Places = S.Set Place
type Edges  = [Edge]

data Automaton = Automaton {
    places       :: Places,
    edges        :: Edges,
    initState    :: Place,
    validating   :: Places,
    counter      :: Int
}

instance Eq Automaton where
  a1 == a2 =
      (places a1 == places a2)
      && (length (intersect (edges a1) (edges a2)) == length (edges a1))
      && (initState a1 == initState a2)
      && (validating a1 == validating a2)

emptyAutomaton :: Automaton
emptyAutomaton = Automaton { places = S.empty, edges = [], validating = S.empty, initState = trap, counter = 0 }

instance Show Automaton where
  show aut = 
      "digraph {"
      ++ "\n  rankdir=LR;"
      ++ (S.foldl showPlace "" $ places aut)
      ++ (foldl showEdge "" $ edges aut)
      ++ "\n}"
      where showPlace acc pl  = acc ++ "\n  " ++ (plid pl) ++ "[label=\"" ++ show pl ++ mstar pl ++ "\"];"
            showEdge  acc edg = acc ++ "\n  " ++ (plid $ source edg) ++ " -> " ++ (plid $ desti edg) ++ "[label=\"" ++ (show $ label edg) ++ "\"];"
            plid pl = "pl_" ++ (show $ ident pl)
            mstar pl = if S.member pl (validating aut) then "*" else ""





