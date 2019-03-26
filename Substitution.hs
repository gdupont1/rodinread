{-# LANGUAGE MultiParamTypeClasses #-}
module Substitution where

--import Text.Read
import Formula
import Substitution.Automata
import Substitution.Automata.Template
import qualified Substitution.Automata.Template.Parser as TP
import qualified Substitution.Automata.Reader as R
import qualified Substitution.Automata.Parser as P
import Data.List (intercalate,sortBy,group,partition)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.Maybe (Maybe(..),isJust,fromJust)
import Ascii
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

data Rule = Rule {
    rule_pattern :: P.Pattern,
    rule_automaton :: Automaton,
    rule_template :: Template
}
data SubstitutionTable = SubstitutionTable [Rule]

instance Show Rule where
  show (Rule patt _ temp) = "@" ++ (show patt) ++ "@ => @" ++ (foldl (++) "" $ map show temp) ++ "@"

instance Eq Rule where
  (Rule _ au te) == (Rule _ au' te') = (au == au') && (te == te')

instance Show SubstitutionTable where
  show (SubstitutionTable l) = 
      intercalate "\n" $ map show l

class Substituable a where
  substitute :: SubstitutionTable -> a -> Either SubstitutionError a
  substitute _ = return . id

substituteAll :: Substituable a => SubstitutionTable -> [a] -> Either SubstitutionError [a]
substituteAll st = sequence . map (substitute st)

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

data SubstitutionError = SubstitutionError {
    se_rule :: Rule,
    se_tokens :: [Token],
    se_message :: String
}

instance Show SubstitutionError where
  show se =
      "Error when executing rule "
      ++ (show $ se_rule se) ++ " on sequence '"
      ++ (foldl (++) "" $ map showAscii $ se_tokens se) ++ "': "
      ++ (se_message se)

substituteFormula :: SubstitutionTable -> [Token] -> Either SubstitutionError [Token]
substituteFormula (SubstitutionTable []) input = Right input
substituteFormula _ [] = Right []
substituteFormula table input@(i:is) =
    case lookupPattern table input of
      Nothing -> (i:) <$> substituteFormula table is
      Just rm ->
          case fill (matched rm) (captures rm) (rule_template $ rule rm) of
            Left err  -> Left $ SubstitutionError (rule rm) (matched rm) $ "at '" ++ tpe_fragment err ++ "': " ++ tpe_message err
            Right tks -> substituteFormula table (tks ++ remaining rm)

data SubstitutionTableError = SubstitutionTableError {
    ste_line :: Int,
    ste_fragment :: String,
    ste_message :: String
}

instance Show SubstitutionTableError where
  show ste =
      "Error when parsing substitution table: "
      ++ (if ste_line ste > 0 then "on line " ++ (show $ ste_line ste)
        ++ (if not $ null $ ste_fragment ste then ", at '" ++ (ste_fragment ste) ++ "'" else "")
        ++ ": " else "")
      ++ (ste_message ste)

data SubstitutionTableWarning = SubstitutionTableWarning {
    stw_line :: Int,
    stw_message :: String
}

instance Show SubstitutionTableWarning where
  show stw =
      "Warning: on line " ++ (show $ stw_line stw) ++ ": " ++ (stw_message stw)

data Wrap e w a = WrapWarnings [w] a | WrapError e

instance (Show w, Show e) => (Show (Wrap e w a)) where
  show (WrapWarnings ws _) = intercalate "\n" $ map show ws
  show (WrapError e) = show e


instance Functor (Wrap e w) where
  fmap f (WrapWarnings ws a) = WrapWarnings ws (f a)
  fmap f (WrapError e) = WrapError e

instance Applicative (Wrap e w) where
  pure a = WrapWarnings [] a
  (WrapWarnings wf f) <*> (WrapWarnings wa a) = WrapWarnings (wf ++ wa) (f a)
  (WrapError e) <*> _ = WrapError e
  _ <*> (WrapError e) = WrapError e

instance Monad (Wrap e w) where
  return = pure
  (WrapWarnings wa a) >>= f =
      case f a of
        WrapWarnings wb b -> WrapWarnings (wa ++ wb) b
        WrapError e -> WrapError e
  (WrapError e) >>= f = WrapError e


newtype WrapT e w m a = WrapT { runWrapT :: m (Wrap e w a) }

instance MonadTrans (WrapT e w) where
  -- lift :: Monad m => m a -> WrapT m a
  lift = WrapT . liftM (WrapWarnings [])

instance Monad m => Functor (WrapT e w m) where
  -- (a -> b) -> WrapT m a -> WrapT m b
  fmap f sa = WrapT $ (runWrapT sa) >>= (\stw -> return $ fmap f stw)

instance Monad m => Applicative (WrapT e w m) where
  -- a -> WrapT m a
  pure = lift . return
  -- WrapT m (a -> b) -> WrapT m a -> Wrap m b
  swf <*> swa = WrapT $ do
      f <- runWrapT swf
      a <- runWrapT swa
      return $ f <*> a

instance Monad m => Monad (WrapT e w m) where
  -- a -> WrapT e w m a
  return = pure
  -- WrapT e w m a -> (a -> WrapT e w m b) -> WrapT e w m b
  swtx >>= fswtb = WrapT $
      runWrapT swtx >>= (\swx ->
          case swx of
            WrapError e -> return $ WrapError e
            WrapWarnings ws a -> runWrapT (fswtb a) >>= transposeM ws
      )
      where transposeM :: Monad m => [w] -> Wrap e w b -> m (Wrap e w b)
            transposeM _  (WrapError    e    ) = return $ WrapError e
            transposeM ws (WrapWarnings ws' b) = return $ WrapWarnings (ws ++ ws') b


mapWrapT :: (Monad m, Monad n) => (m (Wrap e w a) -> n (Wrap e w b)) -> WrapT e w m a -> WrapT e w n b
mapWrapT f = WrapT . f . runWrapT

wrapT :: Monad m => Wrap e w a -> WrapT e w m a
wrapT = WrapT . return

type STWrap = Wrap SubstitutionTableError SubstitutionTableWarning
type STWrapT = WrapT SubstitutionTableError SubstitutionTableWarning

failwith :: e -> Wrap e w a
failwith = WrapError

failwith' :: Int -> String -> String -> STWrap a
failwith' line fragment message = failwith $ SubstitutionTableError line fragment message

pushw :: w -> Wrap e w ()
pushw warning = WrapWarnings [warning] ()

pushw' :: Int -> String -> STWrap ()
pushw' line msg = pushw $ SubstitutionTableWarning line msg

fromLine :: Int -> String -> STWrap Rule
fromLine id part = do
    (exleft,exright) <- extract part
    (pa,au) <- parsePattern exleft
    templ   <- parseTemplate exright
    return Rule {
      rule_pattern = pa,
      rule_automaton = au,
      rule_template = templ
    }
    where extract :: String -> STWrap (String,String)
          extract part = do
              (first,rem) <- getfragment part
              if null rem then
                  failwith' id part $ "missing template side of substitution rule"
              else do
                  (second,_) <- getfragment rem
                  return (first,second)
          getfragment :: String -> STWrap (String,String)
          getfragment p =
              case dropWhile (/= '@') p of
                []     -> failwith' id p $ "syntax error: no '@' detected"
                (c:cs) ->
                    let (frag,rem) = span (/= '@') cs in
                        if null rem || head rem /= '@' then
                            failwith' id p $ "syntax error: unclosed '@'"
                        else
                            return (frag, tail rem)
          parsePattern :: String -> STWrap (P.Pattern,Automaton)
          parsePattern extract =
              case P.compile extract of
                Left err ->
                    failwith' id extract $ "'" ++ P.ape_fragment err ++ "' (" ++ (show $ P.ape_position err) ++ "): " ++ P.ape_message err
                Right res -> return res
          parseTemplate :: String -> STWrap Template
          parseTemplate extract =
              case TP.template extract of
                Left err ->
                    failwith' id extract $ "'" ++ TP.tpe_fragment err ++ "': " ++ TP.tpe_message err
                Right res -> return res

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



