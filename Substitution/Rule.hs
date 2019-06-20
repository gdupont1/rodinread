module Substitution.Rule where

import Substitution.Template
import Substitution.Automata.Parser (Pattern)
import Substitution.Automata
import Data.List (intercalate, partition)

data RuleOptions = RuleOptions {
    ro_chomp :: Bool
}

default_options :: RuleOptions
default_options = RuleOptions False

instance Show RuleOptions where
  show (RuleOptions ch) = (if ch then "c" else "")

data Rule = Rule {
    rule_pattern :: Pattern,
    rule_automaton :: Automaton,
    rule_template :: Template,
    rule_options :: RuleOptions
}

rule_option :: (RuleOptions -> a) -> Rule -> a
rule_option f = f . rule_options


instance Show Rule where
  show (Rule patt _ temp ro) = "@" ++ (show patt) ++ "@ => @" ++ (foldl (++) "" $ map show temp) ++ "@" ++ (show ro)

instance Eq Rule where
  r == r' = (rule_automaton r == rule_automaton r') && (rule_template r == rule_template r')

data SubstitutionTable = SubstitutionTable [Rule]

instance Show SubstitutionTable where
  show (SubstitutionTable l) = 
      intercalate "\n" $ map show l


emptySubstitutionTable :: SubstitutionTable
emptySubstitutionTable = SubstitutionTable []

addrule :: SubstitutionTable -> Rule -> SubstitutionTable
addrule (SubstitutionTable rs) r = SubstitutionTable (rs ++ [r])

empty :: SubstitutionTable -> Bool
empty (SubstitutionTable x) = null x

union :: SubstitutionTable -> SubstitutionTable -> SubstitutionTable
union (SubstitutionTable t1) (SubstitutionTable t2) = SubstitutionTable (t1 ++ t2)

collisions :: SubstitutionTable -> [[Rule]]
collisions (SubstitutionTable table) =
    filter ((> 1).length) $ takeAppart table
    where takeAppart [] = []
          takeAppart (t:ts) =
              let (same,others) = partition (== t) ts in
                  (t:same):(takeAppart others)


