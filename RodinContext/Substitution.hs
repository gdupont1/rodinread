module RodinContext.Substitution where

import Formula.Substitution
import RodinContext
import Substitution

instance Substituable ExtendsContext

instance Substituable Axiom where
  substitute st axm =
      (substitute st $ axPred axm) >>= (\x -> return $ axm { axPred = x })

instance Substituable Constant
instance Substituable CarrierSet

instance Substituable Context where
  substitute st ctx = do
      cExt <- substituteAll st $ ctxExtends   ctx
      cAxi <- substituteAll st $ ctxAxioms    ctx
      cCon <- substituteAll st $ ctxConstants ctx
      cSet <- substituteAll st $ ctxSets      ctx
      return $ ctx { ctxExtends = cExt, ctxAxioms = cAxi, ctxConstants = cCon, ctxSets = cSet }


