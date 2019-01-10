{-|
Module      : xxx
Description : ...
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

extended description...
-}
module RodinTheory.TeX where

import Util
import TeX
import Formula.Tokenizer
import Formula.TeX
import RodinTheory

instance ShowTeX NotationType where
  showTeX Prefix = ""
  showTeX Infix = "INFIX "

instance ShowTeX OperatorProp where
  showTeX (OperatorProp as co fo no) =
      (if as then " " ++ italic "associative" else "") ++
      (if co then " " ++ italic "commutative" else "") ++
      (if fo then " <expression>" else " <predicate>") ++
      " " ++ showTeX no

instance ShowTeX OperatorArgument where
  showTeX (OperatorArgument ex id) =
      id ++ ": " ++ printTeX ex

instance ShowTeX OperatorWDCondition where
  showTeX (OperatorWDCondition pr) = printTeX pr

instance ShowTeX OperatorDirectDefinition where
  showTeX (OperatorDirectDefinition fo) =
      "\n" ++ ind 3 ++ "direct definition " ++ printTeXLines 4 fo

instance ShowTeX RecursiveDefinitionCase where
  showTeX (RecursiveDefinitionCase ex fo) =
      "\n" ++ ind 4 ++ printTeX ex ++ " => " ++ printTeX fo

instance ShowTeX OperatorRecursiveDefinition where
  showTeX (OperatorRecursiveDefinition ia ca) =
      "\n" ++ ind 3 ++ "case " ++ ia ++ ":\n" ++ (texlist "" ca)

instance ShowTeX ImportTheory where
  showTeX (ImportTheory th) = th

instance ShowTeX ImportTheoryProject where
  showTeX (ImportTheoryProject pr th) =
      "\n" ++ ind 2 ++ pr ++ " THEORIES " ++ (texlist "," th)

instance ShowTeX TypeParameter where
  showTeX (TypeParameter ti) = ti

instance ShowTeX ConstructorArgument where
  showTeX (ConstructorArgument id ty) =
      id ++ ":" ++ printTeX ty

instance ShowTeX DataTypeConstructor where
  showTeX (DataTypeConstructor id ar) =
      "\n" ++ ind 3 ++ id ++ "(" ++ (texlist "," ar) ++ ")"

instance ShowTeX TypeArgument where
  showTeX (TypeArgument ty) = printTeX ty

instance ShowTeX DataTypeDefinition where
  showTeX (DataTypeDefinition id ar co) =
      "\n" ++ ind 2 ++ id ++ (if not $ null ar then "(" ++ texlist "," ar ++ ")" else "") ++
      (if not $ null co then "\n" ++ ind 2 ++ "CONSTRUCTORS" ++ texlist "" co else "")

instance ShowTeX NewOperatorDefinition where
  showTeX (NewOperatorDefinition la pr ar wd di re) =
      "\n" ++ ind 2 ++ (bold $ escape_ la) ++ showTeX pr ++ "(" ++ (texlist "," ar) ++ ")" ++
      (if not $ null wd then "\n" ++ ind 3 ++ "well-definedness " ++ texlist "," wd else "") ++
      (if not $ null di then texlist "" di else "") ++
      (if not $ null re then texlist "" re else "")

instance ShowTeX AxiomaticTypeDefinition where
  showTeX (AxiomaticTypeDefinition id) = id


instance ShowTeX AxiomaticOperatorDefinition where
  showTeX (AxiomaticOperatorDefinition pr la ty ar wd) =
      "\n" ++ ind 3 ++ (bold $ escape_ la) ++ showTeX pr ++ "(" ++ (texlist "," ar) ++ ") : " ++ printTeX ty ++
      (if not $ null wd then "\n" ++ ind 4 ++ "well-definedness " ++ (texlist "," wd) else "")

instance ShowTeX AxiomaticDefinitionAxiom where
  showTeX (AxiomaticDefinitionAxiom la pr) =
      "\n" ++ ind 3 ++ (italic $ escape_ la) ++ ": " ++ printTeXLines 4 pr

instance ShowTeX AxiomaticDefinitionsBlock where
  showTeX (AxiomaticDefinitionsBlock la ty de ax) =
      ind 1 ++ la ++ ":" ++
      (if not $ null ty then "\n" ++ ind 2 ++ "TYPES " ++ (texlist ", " ty) else "") ++
      (if not $ null de then "\n" ++ ind 2 ++ "OPERATORS" ++ (texlist "" de) else "") ++
      (if not $ null ax then "\n" ++ ind 2 ++ "AXIOMS" ++ (texlist "" ax) else "")


instance ShowTeX Theorem where
  showTeX (Theorem na pr) =
      "\n" ++ ind 2 ++ (italic $ escape_ na) ++ ": " ++ printTeXLines 3 pr

instance ShowTeX MetaVariable where
  showTeX (MetaVariable id ty) =
      "\n" ++ ind 3 ++ id ++ ": " ++ printTeX ty

instance ShowTeX InferenceGiven where
  showTeX (InferenceGiven pr _) = printTeX pr

instance ShowTeX InferenceInfer where
  showTeX (InferenceInfer pr) = printTeX pr

instance ShowTeX InferenceRule where
  showTeX (InferenceRule la ap de gi ie) =
      "\n" ++ ind 3 ++ (italic $ escape_ la) ++ ": " ++ (texlist "," gi) ++ " $\\vdash$ " ++ (texlist "," ie)

instance ShowTeX RewriteRuleRHS where
  showTeX (RewriteRuleRHS la pr fo) =
      "\n" ++ ind 4 ++ la ++ ": " ++ printTeX pr ++ " $\\Rightarrow$ " ++ printTeX fo

instance ShowTeX RewriteRule where
  showTeX (RewriteRule la ap co de ls rs) =
      "\n" ++ ind 3 ++ (italic $ escape_ la) ++ ": " ++ printTeX ls ++ (texlist "" rs)

instance ShowTeX ProofRulesBlock where
  showTeX (ProofRulesBlock la mv ir rr) =
      "\n" ++ ind 2 ++ la ++ ":" ++
      (if not $ null mv then "\n" ++ ind 2 ++ "Metavariables" ++ (texlist "" mv) else "") ++
      (if not $ null rr then "\n" ++ ind 2 ++ "Rewrite Rules" ++ (texlist "" rr) else "") ++
      (if not $ null ir then "\n" ++ ind 2 ++ "Inference Rules" ++ (texlist "" ir) else "")

instance ShowTeX Theory where
  showTeX (Theory im ty da op ax th pr) =
      "THEORY" ++
      (if not $ null im then "\n" ++ ind 1 ++ "IMPORT THEORY PROJECTS" ++ (texlist "" im) else "") ++
      (if not $ null ty then "\n" ++ ind 1 ++ "TYPE PARAMETERS "       ++ (texlist "," ty) else "") ++
      (if not $ null da then "\n" ++ ind 1 ++ "DATA TYPES"             ++ (texlist "" da) else "") ++
      (if not $ null op then "\n" ++ ind 1 ++ "OPERATORS"              ++ (texlist "" op) else "") ++
      (if not $ null ax then "\n" ++ ind 1 ++ "AXIOMATIC DEFINITIONS"  ++ (texlist "" ax) else "") ++
      (if not $ null th then "\n" ++ ind 1 ++ "THEOREMS"               ++ (texlist "" th) else "") ++
      (if not $ null pr then "\n" ++ ind 1 ++ "PROOF RULES"            ++ (texlist "" pr) else "") ++
      "\nEND"





