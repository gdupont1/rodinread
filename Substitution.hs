{-# LANGUAGE MultiParamTypeClasses #-}
module Substitution where

--import Text.Read
import Wrap
import Ascii
import Formula
import qualified Formula.Util as FU
import qualified Formula.Tokenizer as FT
import Substitution.Automata
import Substitution.Template
import Substitution.Message
import Substitution.Rule
import qualified Substitution.Template.Parser as TP
import qualified Substitution.Automata.Reader as R
import qualified Substitution.Automata.Parser as P
import Data.List (intercalate,sortBy,group,partition)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.Maybe (Maybe(..),isJust,fromJust)
import Control.Monad (foldM,forM_)
import Control.Monad.Trans.Class (lift)


class Substituable a where
  substitute :: SubstitutionTable -> a -> SWrap a
  substitute _ = return . id

substituteAll :: Substituable a => SubstitutionTable -> [a] -> SWrap [a]
substituteAll st = sequence . map (substitute st)

data RuleMatch = RuleMatch {
    rule :: Rule,
    matched :: [Token],
    remaining :: [Token],
    captures :: [[Token]]
}

lookupPattern :: SubstitutionTable -> [Token] -> Maybe RuleMatch
lookupPattern (SubstitutionTable table) formula =
    firstValidating $ map match table
    where match rl = (rl, R.stepuntilvalidating $ R.reader (rule_automaton rl) formula) 
          firstValidating [] = Nothing
          firstValidating ((rl,rd):ts)
              | R.validatingState rd = Just $ RuleMatch {
                  rule      = rl,
                  matched   = R.readTokens rd,
                  remaining = R.remainingTokens rd,
                  captures  = R.captures rd
              }
              | otherwise = firstValidating ts

processOptions :: RuleMatch -> SWrap RuleMatch
processOptions rm =
    foldM applyIf rm [(ro_chomp,chomppy)]
    where applyIf :: RuleMatch -> (RuleOptions -> Bool, RuleMatch -> SWrap RuleMatch) -> SWrap RuleMatch
          applyIf rm (get,fun)
              | rule_option get (rule rm) = fun rm
              | otherwise                 = return $ rm
          chomppy rm =
              return $ rm { matched = chomp $ matched rm, captures = map chomp $ captures rm }
          chomp tks =
              takeout $ dropWhile FU.isSpace $ tks
          takeout = foldr (\x -> \acc -> if null acc && FU.isSpace x then [] else x:acc) []

match :: Rule -> [Token] -> Maybe RuleMatch
match rl formula =
    let rd = R.stepuntilvalidating $ R.reader (rule_automaton rl) formula in
        if R.validatingState rd then
            Just $ RuleMatch {
                rule      = rl,
                matched   = R.readTokens rd,
                remaining = R.remainingTokens rd,
                captures  = R.captures rd
            }
        else Nothing


trySubstitute1 :: Rule -> [Token] -> SWrap (Bool,[Token])
trySubstitute1 rl []      = return $ (False,[])
trySubstitute1 rl formula@(f:fs) =
    case match rl formula of
      Nothing -> trySubstitute1 rl fs >>= (\(b,tks) -> return (b,f:tks))
      Just rm -> do
          rm' <- processOptions rm
          tks <- transpose (te_to_se rm') (tw_to_sw rm') $ fill (matched rm) (captures rm) (rule_template $ rule rm')
          (_,tks') <- trySubstitute1 rl (tks ++ remaining rm')
          return (True,tks')
    where te_to_se :: RuleMatch -> TemplateError -> SubstitutionError
          te_to_se rm err = SubstitutionError (rule rm) (matched rm) $ "at '" ++ tpe_fragment err ++ "': " ++ tpe_message err
          tw_to_sw :: RuleMatch -> [TemplateWarning] -> [SubstitutionWarning]
          tw_to_sw _  _   = []

trySubstitute :: Rule -> [Token] -> SWrap (Bool,[Token])
trySubstitute rl formula =
    let l = tail $ iterate trys (return $ (True,formula)) in
        if not $ cue $ head l then -- stopped after 1st iteration
            head l -- something like (False,...)
        else do -- went on
            (_,tks) <- head $ dropWhile cue l
            return (True,tks)
    where trys swt = swt >>= (\(_,tks) -> trySubstitute1 rl tks)
          cue = fst . getContent (False,[]) -- returns True if iteration should go on i.e. if the element differs from the previous one

trySubstituteAll1 :: [Rule] -> [Token] -> SWrap (Bool,[Token])
trySubstituteAll1 rls formula =
    foldM (\(bl,tks) -> \rl -> lor bl $ trySubstitute rl tks) (False,formula) rls
    where lor :: Bool -> SWrap (Bool,[Token]) -> SWrap (Bool,[Token])
          lor b sw = sw >>= (\(b',ct) -> return (b || b',ct))

trySubstituteAll :: [Rule] -> [Token] -> SWrap [Token]
trySubstituteAll rls formula =
    let l = tail $ iterate trysa (return $ (True,formula)) in do
        (_,tks) <- head $ dropWhile cue l
        return tks
    where trysa swt = swt >>= (\(_,tks) -> trySubstituteAll1 rls tks)
          cue = fst . getContent (False,[]) -- returns True if iteration should go on i.e. if the element differs from the previous one

substituteFormula :: [Token] -> SubstitutionTable -> SWrap [Token]
substituteFormula formula (SubstitutionTable rls) = trySubstituteAll rls formula

fromLine :: Int -> String -> STWrap Rule
fromLine id part = do
    (exleft,exright,extra) <- extract part
    (pa,au) <- parsePattern exleft
    templtk <- parseTemplateTokens exright
    templ   <- parseTemplate exright templtk
    opts    <- parseOptions extra
    if doesMatch au templtk then
        pushw' SubstitutionTableWarning id "Rule template matches rule pattern! This can result in infinite loop when processing formulas." ()
    else return ()
    return $ Rule {
      rule_pattern = pa,
      rule_automaton = au,
      rule_template = templ,
      rule_options = opts
    }
    where extract :: String -> STWrap (String,String,String)
          extract part = do
              (first,rem) <- getfragment part
              if null rem then
                  failwith' st_err (id,part) $ "missing template side of substitution rule"
              else do
                  (second,extra) <- getfragment rem
                  return (first,second,filter (not . isSpace) extra)
          getfragment :: String -> STWrap (String,String)
          getfragment p =
              case dropWhile (/= '@') p of
                []     -> failwith' st_err (id,p) $ "syntax error: no '@' detected"
                (c:cs) ->
                    let (frag,rem) = span (/= '@') cs in
                        if null rem || head rem /= '@' then
                            failwith' st_err (id,p) $ "syntax error: unclosed '@'"
                        else
                            return (frag, tail rem)
          parsePattern :: String -> STWrap (P.Pattern,Automaton)
          parsePattern extract = transpose (ape_to_ste extract) (apw_to_stw extract) $ P.compile extract
          parseTemplateTokens :: String -> STWrap [Token]
          parseTemplateTokens extract = transpose (fpe_to_ste extract) (fpw_to_stw extract) $ FT.tokenize showAscii extract
          parseTemplate :: String -> [Token] -> STWrap Template
          parseTemplate extract tks = transpose (te_to_ste extract) (tw_to_stw extract) $ TP.template' tks
          parseOptions :: String -> STWrap RuleOptions
          parseOptions extract = foldM parseOption default_options extract
          parseOption :: RuleOptions -> Char -> STWrap RuleOptions
          parseOption ros 'c' = return $ ros { ro_chomp = True }
          parseOption ros o   = pushw' SubstitutionTableWarning id ("Unknown option " ++ show o) ros
          te_to_ste :: String -> TemplateError -> SubstitutionTableError
          te_to_ste part err =
              SubstitutionTableError id part $ "'" ++ tpe_fragment err ++ "': " ++ tpe_message err
          tw_to_stw :: String -> [TemplateWarning] -> [SubstitutionTableWarning]
          tw_to_stw _ _ = []
          ape_to_ste :: String -> P.AutomatonParseError -> SubstitutionTableError
          ape_to_ste part err = 
              SubstitutionTableError id part $ "'" ++ P.ape_fragment err ++ "' (" ++ (show $ P.ape_position err) ++ "): " ++ P.ape_message err
          apw_to_stw :: String -> [P.AutomatonParseWarning] -> [SubstitutionTableWarning]
          apw_to_stw _ _ = []
          fpe_to_ste :: String -> FT.FormulaParseError -> SubstitutionTableError
          fpe_to_ste part pe =
              SubstitutionTableError id (FT.fpe_fragment pe ++ " (" ++ (show $ FT.fpe_position pe) ++ ")") (FT.fpe_message pe)
          fpw_to_stw :: String -> [FT.FormulaParseWarning] -> [SubstitutionTableWarning]
          fpw_to_stw _ _  = []
          doesMatch :: Automaton -> [Token] -> Bool
          doesMatch aut = 
              R.validatingState . R.stepuntilvalidating . R.reader aut



fromString :: String -> STWrap SubstitutionTable
fromString s =
    foldl readnext (pure emptySubstitutionTable) $ zip [1..] (lines s)
    where readnext :: STWrap SubstitutionTable -> (Int,String) -> STWrap SubstitutionTable
          readnext acc (id,line)
              | null line || all isSpace line || head line == ':' = acc
              | otherwise = addrule <$> acc <*> fromLine id line

fromFile :: FilePath -> STWrapT IO SubstitutionTable
fromFile fp =
    (lift $ readFile fp) >>= (wrapT . fromString)


simplematch :: String -> String -> String -> IO ()
simplematch form patt temp = do
    (p,a) <- cmp
    t     <- tmp 
    f     <- tok
    putStrLn $ "Pattern: "    ++ show p
    putStrLn $ "Automaton:\n" ++ show a
    putStrLn $ "Template: "   ++ show t
    putStrLn $ "Formula: "    ++ show f
    putStrLn $ "================================="
    rd  <- return $ R.reader a f
    putStrLn $ "Reader (init):\n" ++ show rd
    its <- return $ tail $ takeWhile (not . finished) $ iterate R.step rd
    forM_ (zip [1..] its) $ (\(i,s) -> putStrLn ("Reader (step " ++ show i ++ "):") >> putStrLn (show s))
    rd' <- return $ head $ dropWhile (not . finished) $ iterate R.step rd
    putStrLn $ "Reader (final):\n" ++ show rd'
    val <- return $ R.validatingState rd'
    putStrLn $ "Validating? " ++ show val
    if val then do
      putStrLn "Captures:"
      putStrLn $ " $0 \"" ++ show (reverse $ R.readTokens rd') ++ "\""
      forM_ (zip [1..] (R.captures rd')) $ (\(i,tks) -> putStrLn $ " $" ++ show i ++ " \"" ++ show tks ++ "\"")
      doOrFail (\e -> putStrLn $ "Error while replacing: " ++ show e)
               (\r -> putStrLn $ "\nReplacement result: " ++ show r) $
               fill (R.readTokens rd') (R.captures rd') (t)
    else return ()
    putStrLn "==================================="
    putStrLn "Done."
    where cmp = doOrFail (fail . show) return $ P.compile patt
          tmp = doOrFail (fail . show) return $ TP.template temp
          tok = doOrFail (fail . show) return $ FT.tokenize showAscii form
          finished = R.finishedorvalidating
            


