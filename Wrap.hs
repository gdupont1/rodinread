{-|
Module      : Wrap
Description : module for wrapping error-probable process into monads
Copyright   : (c) Guillaume Dupont, 2019
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

This module provides two important types:
  - The Wrap construct wraps a type, associating it with warning messages
  or eventual error.
  This kind of behave like an advanced form of Either, with the difference
  that, in addition to standard error handling, the "right" hand side of
  Wrap also stores multiple warning messages.

  - The WrapT monad transformer allows to combine Wrap with another monad,
  and in particular with IO, as a lot of processes in this project use
  this specific monad (whenever reading a file for example).
  The (WrapT IO) monad is alos useful whenever outputing the messages of
  the Wrap monad (error/warning messages)
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Wrap where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.List (intercalate)

-- | The 'Wrap' type, composed of a raw type (a), a type for errors (e) and a type for warnings (w)
data Wrap e w a =
      WrapWarnings [w] a        -- ^ Raw type + warning messages (~ Right)
    | WrapError e               -- ^ Error message (~ Left)

-- | Show instance for wrap. Very useful to show the actual messages inside the wrap.
-- Note that this does NOT print the *content* of the wrap.
instance (Show w, Show e) => (Show (Wrap e w a)) where
  show (WrapWarnings ws _) = intercalate "\n" $ map show ws
  show (WrapError e) = show e

-- | Functor instance for the wrap. Behaves like expected.
instance Functor (Wrap e w) where
  fmap f (WrapWarnings ws a) = WrapWarnings ws (f a)
  fmap f (WrapError e) = WrapError e

-- | Applicative instance for the wrap. Behaves like expected as for the content.
-- As for the warnings, when performing (f <*> a), the warnings of f are simply
-- concatenated to the warnings of a.
instance Applicative (Wrap e w) where
  -- | @'pure' a@ yields a simple wrapped @a@
  pure a = WrapWarnings [] a
  -- | '(<*>)' implementation
  (WrapWarnings wf f) <*> (WrapWarnings wa a) = WrapWarnings (wf ++ wa) (f a)
  (WrapError e) <*> _ = WrapError e
  _ <*> (WrapError e) = WrapError e

-- | Monad instance for the wrap (relative to its generic type argument @a@).
-- For combination of warning wraps, warnings are simply concatenated. I,
-- particular, when performing @a >>= f@, the resulting wrap has the warnings of
-- a followed by the warnings resulting from the application of .
instance Monad (Wrap e w) where
  -- | 'return' implementation (simply based on 'pure')
  return = pure
  -- | Combination implementation
  (WrapWarnings wa a) >>= f =
      case f a of
        WrapWarnings wb b -> WrapWarnings (wa ++ wb) b
        WrapError e -> WrapError e
  (WrapError e) >>= f = WrapError e

-- | 'WrapT' monad transformer.
-- @e@, @w@ and @a@ are the type arguments of the wrap. @m@ is the monad to combine with.
newtype WrapT e w m a = WrapT { runWrapT :: m (Wrap e w a) }

-- | MonadTrans instance for 'WrapT'. Lifting behaves as expected.
instance MonadTrans (WrapT e w) where
  -- lift :: Monad m => m a -> WrapT m a
  -- | 'lift' transforms a regular monad into a @WrapT m@ combined monad.
  -- The lifted monad is a simple wrapped content with no warnings.
  lift = WrapT . liftM (WrapWarnings [])

-- | Functor instance of the @WrapT e w m@ transformer, provied @m@ is a monad.
-- @m@ is required to be a monad as the implementation of 'fmap' uses the monadic
-- composition operator.
instance Monad m => Functor (WrapT e w m) where
  -- (a -> b) -> WrapT m a -> WrapT m b
  fmap f sa = WrapT $ (runWrapT sa) >>= (\stw -> return $ fmap f stw)

-- | Applicative instance of the @WrapT e w m@ transformer, provided @m@ is a monad.
-- @m@ is required to be a monad as the implementation of 'pure' uses 'return' and
-- the implementation of '(<*>)' uses do-notation (hence @>>=@ operator).
instance Monad m => Applicative (WrapT e w m) where
  -- a -> WrapT m a
  pure = lift . return
  -- WrapT m (a -> b) -> WrapT m a -> Wrap m b
  swf <*> swa = WrapT $ do
      f <- runWrapT swf
      a <- runWrapT swa
      return $ f <*> a

-- | Monad instance of the @WrapT e w m@ transformer, provided @m@ is a monad.
-- Monadic composition simply combines @Wrap@'s composition and @m@'s. In other
-- words, @>>=@ will carry out @m@ monadic composition, and then wrap the result
-- in a @Wrap@ by applying @Wrap@'s monadic composition.
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

-- | Utility monad transformation function. Takes a transformation function on monad-ed wraps and
-- a combined wrap and yield a new combined wrap with the new monad.
mapWrapT :: (Monad m, Monad n) => (m (Wrap e w a) -> n (Wrap e w b)) -> WrapT e w m a -> WrapT e w n b
mapWrapT f = WrapT . f . runWrapT

-- | Build a @WrapT e w m a@ monad from a simple @Wrap@ (monad @m@ is to be inferred)
wrapT :: Monad m => Wrap e w a -> WrapT e w m a
wrapT = WrapT . return

-- | Test if the wrapper is in fact an error
isError :: Wrap e w a -> Bool
isError (WrapError _) = True
isError _ = False

-- | Retrieve an error from the wrapper
getError :: Wrap e w a -> e
getError (WrapError e) = e

-- | Try to retrieve an error from the wrapper
tryGetError :: Wrap e w a -> Maybe e
tryGetError (WrapError e) = Just e
tryGetError _ = Nothing

-- | Test if the wrapper is in fact an element + warnings
isOk :: Wrap e w a -> Bool
isOk (WrapWarnings _ _) = True
isOk _ = False

-- | Retrieve the warnings from the wrapper
getWarnings :: Wrap e w a -> [w]
getWarnings (WrapWarnings ws _) = ws

-- | Retrieve the content of the wrapper
getContent' :: Wrap e w a -> a
getContent' (WrapWarnings _ a) = a

-- | Retrieve the content of the wrapper or the default value provided
getContent :: a -> Wrap e w a -> a
getContent _ (WrapWarnings _ a) = a
getContent a (WrapError _)      = a

-- | Try to retrieve warnings from the wrapper
tryGetWarnings :: Wrap e w a -> Maybe [w]
tryGetWarnings (WrapWarnings ws _) = Just ws
tryGetWarnings _ = Nothing

-- | Try to retrieve content from the wrapper
tryGetContent :: Wrap e w a -> Maybe a
tryGetContent (WrapWarnings _ a) = Just a
tryGetContent _ = Nothing

-- | Initiate a computation if possible or fail
doOrFail :: (e -> m b) -> (a -> m b) -> Wrap e w a -> m b
doOrFail _       theDo (WrapWarnings _ a) = theDo a
doOrFail theFail _     (WrapError      e) = theFail e

-- | Warn, then 'doOrFail'
warnThenDoOrFail :: Monad m => ([w] -> m b) -> (e -> m b) -> (a -> m b) -> Wrap e w a -> m b
warnThenDoOrFail _ theFail _ (WrapError e) = theFail e
warnThenDoOrFail theWarn _ theDo (WrapWarnings ws a) =
    (theWarn ws) >> (theDo a)


-- | Fail method, building basically a @WrapError@ with given error @e@.
-- It is especially convenient in do-notation blocks :
-- > do
-- >   a <- ...
-- >   if a ... then failwith "Error: ..."
-- >   else return ()
-- >   ...
failwith :: e -> Wrap e w a
failwith = WrapError

-- | Convenient extension of the 'failwith' method aimed at improving readability.
-- It simply allow writing such thing as:
-- > failwith error_constructor (opt1,opt2,...) message
-- wich matches error management (message + contextual info) most of the time.
failwith' :: (eo -> em -> e) -> eo -> em -> Wrap e w a
failwith' cstr opt msg = failwith $ cstr opt msg

-- | Push a warning message to the wrap method. It is intended to be used in monadic
-- composition and typically in do-notation blocks:
-- > a <- comp
-- > pushw "Warning: ..."
-- > b <- comp a
pushw :: w -> a -> Wrap e w a
pushw warning = WrapWarnings [warning]

-- | Convenient extension for the 'pushw' method aimed at improving readability.
-- It works exactly like 'failwith\'' by accepting some kind of warning constructor,
-- with message and contextual info.
pushw' :: (wo -> wm -> w) -> wo -> wm -> a -> Wrap e w a
pushw' cstr opt msg = pushw (cstr opt msg)

-- | Transpose a wrapper to another wrapper by converting its error or warnings.
-- Wrapper's content remains unchanged.
transpose :: (e1 -> e2) -> ([w1] -> [w2]) -> Wrap e1 w1 a -> Wrap e2 w2 a
transpose etrans _ (WrapError e) = WrapError $ etrans e
transpose _ wtrans (WrapWarnings ws a) = WrapWarnings (wtrans ws) a

-- | Raise the severity of a wrapper, transforming it to a WrapError.
-- If the wrapper is already a WrapError it will remain unchanged
raise :: ([w] -> e) -> Wrap e w a -> Wrap e w a
raise f (WrapError e) = WrapError e
raise f (WrapWarnings ws a) = WrapError $ f ws





