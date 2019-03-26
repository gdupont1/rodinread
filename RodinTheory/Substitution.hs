module RodinTheory.Substitution where

import Formula.Substitution
import Substitution
import RodinTheory

instance Substituable OperatorProp

instance Substituable OperatorArgument where
  substitute st arg = 
      (substitute st $ expression arg) >>= (\x -> return $ arg { expression = x })

instance Substituable OperatorWDCondition where
  substitute st wdc = 
      (substitute st $ predicate wdc) >>= (\x -> return $ wdc { predicate = x })

instance Substituable OperatorDirectDefinition where
  substitute st ddf =
      (substitute st $ formula ddf) >>=  (\x -> return $ ddf { formula = x })

instance Substituable RecursiveDefinitionCase where
  substitute st rdc = do
      caEx <- substitute st $ caseExpression rdc
      caFo <- substitute st $ caseFormula rdc
      return $ rdc { caseExpression = caEx, caseFormula = caFo }

instance Substituable OperatorRecursiveDefinition where
  substitute st ord = 
      (substituteAll st $ cases ord) >>= (\x -> return $ ord { cases = x })

instance Substituable ImportTheory
instance Substituable ImportTheoryProject
instance Substituable TypeParameter

instance Substituable ConstructorArgument where
  substitute st ca = 
      (substitute st $ caType ca) >>= (\x -> return $ ca { caType = x })

instance Substituable DataTypeConstructor where
  substitute st dtc =
      (substituteAll st $ args dtc) >>= (\x -> return $ dtc { args = x })

instance Substituable TypeArgument where
  substitute st ta = 
      (substitute st $ typeArg ta) >>= (\x -> return $ ta { typeArg = x })

instance Substituable DataTypeDefinition where
  substitute st dtd = do
      tyar <- substituteAll st $ typeArguments dtd
      cons <- substituteAll st $ constructors  dtd
      return $ dtd { typeArguments = tyar, constructors = cons }

instance Substituable NewOperatorDefinition where
  substitute st nod = do
      opar <- substituteAll st $ opArgs   nod
      opwd <- substituteAll st $ opWD     nod
      opdi <- substituteAll st $ opDirDef nod
      opre <- substituteAll st $ opRecDef nod
      return $ nod { opArgs = opar, opWD = opwd, opDirDef = opdi, opRecDef = opre }

instance Substituable AxiomaticTypeDefinition

instance Substituable AxiomaticOperatorDefinition where
  substitute st aod = do
      atyp <- substitute    st $ aType   aod
      aopa <- substituteAll st $ aOpArgs aod
      aopw <- substituteAll st $ aOpWD   aod
      return $ aod { aType = atyp, aOpArgs = aopa, aOpWD = aopw }

instance Substituable AxiomaticDefinitionAxiom where
  substitute st ad =
      (substitute st $ aDefPredicate ad) >>= (\x -> return $ ad { aDefPredicate = x })

instance Substituable AxiomaticDefinitionsBlock where
  substitute st adb = do
      adbt <- substituteAll st $ aDefBTypes adb
      adbd <- substituteAll st $ aDefBDef   adb
      adba <- substituteAll st $ aDefBAx    adb
      return $ adb { aDefBTypes = adbt, aDefBDef = adbd, aDefBAx = adba }

instance Substituable Theorem where
  substitute st thm =
      (substitute st $ thPredicate thm) >>= (\x -> return $ thm { thPredicate = x })

instance Substituable MetaVariable where
  substitute st ty =
      (substitute st $ mvType ty) >>= (\x -> return $ ty { mvType = x })

instance Substituable InferenceGiven where
  substitute st ig =
      (substitute st $ givenPredicate ig) >>= (\x -> return $ ig { givenPredicate = x })

instance Substituable InferenceInfer where
  substitute st ii = 
      (substitute st $ inferPredicate ii) >>= (\x -> return $ ii { inferPredicate = x })

instance Substituable InferenceRule where
  substitute st ir = do
      give <- substituteAll st $ given ir
      infe <- substituteAll st $ infer ir
      return $ ir { given = give, infer = infe }

instance Substituable RewriteRuleRHS where
  substitute st rhs = do
      rhsp <- substitute st $ rhsPredicate rhs
      rhsf <- substitute st $ rhsFormula   rhs
      return $ rhs { rhsPredicate = rhsp, rhsFormula = rhsf }

instance Substituable RewriteRule where
  substitute st rr = do
      slhs <- substitute    st $ lhs rr
      srhs <- substituteAll st $ rhs rr
      return $ rr { lhs = slhs, rhs = srhs }

instance Substituable ProofRulesBlock where
  substitute st prb = do
      meta <- substituteAll st $ metaVariables  prb
      infe <- substituteAll st $ inferenceRules prb
      rewr <- substituteAll st $ rewriteRules   prb
      return $ prb { metaVariables = meta, inferenceRules = infe, rewriteRules = rewr }

instance Substituable Theory where
  substitute st th = do
      imps <- substituteAll st $ imports              th
      typa <- substituteAll st $ typeParameters       th
      dade <- substituteAll st $ datatypeDefinitions  th
      oper <- substituteAll st $ operators            th
      axde <- substituteAll st $ axiomaticDefinitions th
      theo <- substituteAll st $ theorems             th
      prru <- substituteAll st $ proofRules           th
      return $ th {
        imports              = imps,
        typeParameters       = typa,
        datatypeDefinitions  = dade,
        operators            = oper,
        axiomaticDefinitions = axde,
        theorems             = theo,
        proofRules           = prru
      }





