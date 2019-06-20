module RodinMachine.Substitution where

import Formula.Substitution
import RodinMachine
import Substitution

instance Substituable RefinesMachine 
instance Substituable SeesContext

instance Substituable Variable where
  substitute st var =
      (substitute st $ vaIdentifier var) >>= (\x -> return $ var { vaIdentifier = x })


instance Substituable Invariant where
  substitute st inv =
      (substitute st $ invPred inv) >>= (\x -> return $ inv { invPred = x })

instance Substituable Variant where
  substitute st var = 
      (substitute st $ varExpression var) >>= (\x -> return $ var { varExpression = x })

instance Substituable RefinesEvent

instance Substituable Parameter where
  substitute st par =
      (substitute st $ paIdentifier par) >>= (\x -> return $ par { paIdentifier = x })


instance Substituable Guard where
  substitute st grd =
      (substitute st $ guPred grd) >>= (\x -> return $ grd { guPred = x })

instance Substituable Witness where
  substitute st wit = do
      wilab <- substitute st $ wiLabel wit
      wipre <- substitute st $ wiPred wit
      return $ wit { wiPred = wipre, wiLabel = wilab }

instance Substituable Action where
  substitute st act =
      (substitute st $ acAssignment act) >>= (\x -> return $ act { acAssignment = x })

instance Substituable Event where
  substitute st evt = do
      evre <- (substituteAll st $ evRefines    evt)
      evpa <- (substituteAll st $ evParameters evt)
      evgu <- (substituteAll st $ evGuards     evt)
      evwi <- (substituteAll st $ evWitnesses  evt)
      evac <- (substituteAll st $ evActions    evt)
      return $ evt { evRefines = evre, evParameters = evpa, evGuards = evgu, evWitnesses = evwi, evActions = evac }

instance Substituable Machine where
  substitute st mac = do
      mare <- (substituteAll st $ maRefines    mac)
      mase <- (substituteAll st $ maSeesCtx    mac)
      mava <- (substituteAll st $ maVariables  mac)
      main <- (substituteAll st $ maInvariants mac)
      mavn <- (substituteAll st $ maVariants   mac)
      maev <- (substituteAll st $ maEvents     mac)
      return $ mac { maRefines = mare, maSeesCtx = mase, maVariables = mava, maInvariants = main, maVariants = mavn, maEvents = maev }



