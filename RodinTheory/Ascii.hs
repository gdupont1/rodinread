module RodinTheory.Ascii where

import Util
import Ascii
import Formula.Tokenizer
import Formula.Ascii
import RodinTheory

instance ShowAscii NotationType where
  showAscii Prefix = ""
  showAscii Infix = "INFIX "

instance ShowAscii OperatorProp where
  showAscii (OperatorProp as co fo no) =
      (if as then " associative" else "") ++
      (if co then " commutative" else "") ++
      (if fo then " expression" else " predicate") ++
      " " ++ showAscii no

instance ShowAscii OperatorArgument where
  showAscii (OperatorArgument ex id) =
      id ++ ": " ++ printAscii ex

instance ShowAscii OperatorWDCondition where
  showAscii (OperatorWDCondition pr) = printAscii pr

instance ShowAscii OperatorDirectDefinition where
  showAscii (OperatorDirectDefinition fo) =
      "\n" ++ ind 3 ++ "direct definition " ++ printAsciiLines 4 fo

instance ShowAscii RecursiveDefinitionCase where
  showAscii (RecursiveDefinitionCase ex fo) =
      "\n" ++ ind 4 ++ printAscii ex ++ " => " ++ printAscii fo

instance ShowAscii OperatorRecursiveDefinition where
  showAscii (OperatorRecursiveDefinition ia ca) =
      "\n" ++ ind 3 ++ "case " ++ ia ++ ":\n" ++ (asciilist "" ca)

instance ShowAscii ImportTheory where
  showAscii (ImportTheory th) = th

instance ShowAscii ImportTheoryProject where
  showAscii (ImportTheoryProject pr th) =
      "\n" ++ ind 2 ++ pr ++ " THEORIES " ++ (asciilist "," th)

instance ShowAscii TypeParameter where
  showAscii (TypeParameter ti) = ti

instance ShowAscii ConstructorArgument where
  showAscii (ConstructorArgument id ty) =
      id ++ ":" ++ printAscii ty

instance ShowAscii DataTypeConstructor where
  showAscii (DataTypeConstructor id ar) =
      "\n" ++ ind 3 ++ id ++ "(" ++ (asciilist "," ar) ++ ")"

instance ShowAscii TypeArgument where
  showAscii (TypeArgument ty) = printAscii ty

instance ShowAscii DataTypeDefinition where
  showAscii (DataTypeDefinition id ar co) =
      "\n" ++ ind 2 ++ id ++ (if not $ null ar then "(" ++ asciilist "," ar ++ ")" else "") ++
      (if not $ null co then "\n" ++ ind 2 ++ "CONSTRUCTORS" ++ asciilist "" co else "")

instance ShowAscii NewOperatorDefinition where
  showAscii (NewOperatorDefinition la pr ar wd di re) =
      "\n" ++ ind 2 ++ la ++ showAscii pr ++ "(" ++ (asciilist "," ar) ++ ")" ++
      (if not $ null wd then "\n" ++ ind 3 ++ "well-definedness " ++ asciilist "," wd else "") ++
      (if not $ null di then asciilist "" di else "") ++
      (if not $ null re then asciilist "" re else "")

instance ShowAscii AxiomaticTypeDefinition where
  showAscii (AxiomaticTypeDefinition id) = id


instance ShowAscii AxiomaticOperatorDefinition where
  showAscii (AxiomaticOperatorDefinition pr la ty ar wd) =
      "\n" ++ ind 3 ++ la ++ showAscii pr ++ "(" ++ (asciilist "," ar) ++ ") : " ++ printAscii ty ++
      (if not $ null wd then "\n" ++ ind 4 ++ "well-definedness " ++ (asciilist "," wd) else "")

instance ShowAscii AxiomaticDefinitionAxiom where
  showAscii (AxiomaticDefinitionAxiom la pr) =
      "\n" ++ ind 3 ++ la ++ ": " ++ printAsciiLines 4 pr

instance ShowAscii AxiomaticDefinitionsBlock where
  showAscii (AxiomaticDefinitionsBlock la ty de ax) =
      ind 1 ++ la ++ ":" ++
      (if not $ null ty then "\n" ++ ind 2 ++ "TYPES " ++ (asciilist ", " ty) else "") ++
      (if not $ null de then "\n" ++ ind 2 ++ "OPERATORS" ++ (asciilist "" de) else "") ++
      (if not $ null ax then "\n" ++ ind 2 ++ "AXIOMS" ++ (asciilist "" ax) else "")


instance ShowAscii Theorem where
  showAscii (Theorem na pr) =
      "\n" ++ ind 2 ++ na ++ ": " ++ printAsciiLines 3 pr

instance ShowAscii MetaVariable where
  showAscii (MetaVariable id ty) =
      "\n" ++ ind 3 ++ id ++ ": " ++ printAscii ty

instance ShowAscii InferenceGiven where
  showAscii (InferenceGiven pr _) = printAscii pr

instance ShowAscii InferenceInfer where
  showAscii (InferenceInfer pr) = printAscii pr

instance ShowAscii InferenceRule where
  showAscii (InferenceRule la ap de gi ie) =
      "\n" ++ ind 3 ++ la ++ ": " ++ (asciilist "," gi) ++ " |- " ++ (asciilist "," ie)

instance ShowAscii RewriteRuleRHS where
  showAscii (RewriteRuleRHS la pr fo) =
      "\n" ++ ind 4 ++ la ++ ": " ++ printAscii pr ++ " => " ++ printAscii fo

instance ShowAscii RewriteRule where
  showAscii (RewriteRule la ap co de ls rs) =
      "\n" ++ ind 3 ++ la ++ ": " ++ printAscii ls ++ (asciilist "" rs)

instance ShowAscii ProofRulesBlock where
  showAscii (ProofRulesBlock la mv ir rr) =
      "\n" ++ ind 2 ++ la ++ ":" ++
      (if not $ null mv then "\n" ++ ind 2 ++ "Metavariables" ++ (asciilist "" mv) else "") ++
      (if not $ null rr then "\n" ++ ind 2 ++ "Rewrite Rules" ++ (asciilist "" rr) else "") ++
      (if not $ null ir then "\n" ++ ind 2 ++ "Inference Rules" ++ (asciilist "" ir) else "")

instance ShowAscii Theory where
  showAscii (Theory im ty da op ax th pr) =
      "THEORY" ++
      (if not $ null im then "\n" ++ ind 1 ++ "IMPORT THEORY PROJECTS" ++ (asciilist "" im) else "") ++
      (if not $ null ty then "\n" ++ ind 1 ++ "TYPE PARAMETERS "       ++ (asciilist "," ty) else "") ++
      (if not $ null da then "\n" ++ ind 1 ++ "DATA TYPES"             ++ (asciilist "" da) else "") ++
      (if not $ null op then "\n" ++ ind 1 ++ "OPERATORS"              ++ (asciilist "" op) else "") ++
      (if not $ null ax then "\n" ++ ind 1 ++ "AXIOMATIC DEFINITIONS"  ++ (asciilist "" ax) else "") ++
      (if not $ null th then "\n" ++ ind 1 ++ "THEOREMS"               ++ (asciilist "" th) else "") ++
      (if not $ null pr then "\n" ++ ind 1 ++ "PROOF RULES"            ++ (asciilist "" pr) else "") ++
      "\nEND"





