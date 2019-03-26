module RodinPO.Substitution where

import Formula.Substitution
import Substitution
import RodinPO

instance Substituable POIdentifier where
  substitute st poi = 
      (substitute st $ poiType poi) >>= (\x -> return $ poi { poiType = x })

instance Substituable POPredicate where
  substitute st pop = 
      (substitute st $ popPredicate pop) >>= (\x -> return $ pop { popPredicate = x })

instance Substituable POPredicateSet where
  substitute st pops = do
      poid <- (substituteAll st $ popsIdentifiers pops)
      popr <- (substituteAll st $ popsPredicates  pops)
      return $ pops { popsIdentifiers = poid, popsPredicates = popr }

instance Substituable POSource 
instance Substituable POSelHint 

instance Substituable POSequent where
  substitute st pos = do
      pprs <- (substituteAll st $ posPredicateSets pos)
      ppre <- (substituteAll st $ posPredicates    pos)
      psou <- (substituteAll st $ posSources       pos)
      psel <- (substituteAll st $ posSelHints      pos)
      return $ pos { posPredicateSets = pprs, posPredicates = ppre, posSources = psou, posSelHints = psel }

instance Substituable POFile where
  substitute st pof = do
      pofp <- (substituteAll st $ pofPredicateSets pof)
      pofs <- (substituteAll st $ pofSequents      pof)
      return $ pof { pofPredicateSets = pofp, pofSequents = pofs }




