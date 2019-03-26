{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Formula.Substitution where

import Formula
import Substitution

instance Substituable [Token] where
  substitute st fo = substituteFormula st fo


