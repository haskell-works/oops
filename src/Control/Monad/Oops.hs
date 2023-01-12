{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Oops
  ( -- * MTL/transformer utilities
    catchFM,
    catchM,

    throwFM,
    throwM,

    snatchFM,
    snatchM,

    runOops,
    runOops0,
    runOops1,
    suspendM,

    catchAsLeftM,
    catchAsNothingM,
    catchAndExitFailureM,

    throwLeftM,
    throwNothingM,
    throwNothingAsM,

    throwPureLeftM,
    throwPureNothingM,
    throwPureNothingAsM,

    leftM,
    nothingM,

    recoverM,
    recoverOrVoidM,

    DV.CouldBeF (..),
    DV.CouldBe  (..),
    DV.CouldBeAnyOfF,
    DV.CouldBeAnyOf,
    DV.Variant,
    DV.VariantF(..),

  ) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (mapExceptT, runExceptT)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Variant (Catch, CatchF(..), CouldBe, CouldBeF(..), Variant, VariantF, preposterous)
import Data.Void (Void, absurd)

import qualified Data.Variant as DV
import qualified System.Exit  as IO

-- | When working in some monadic context, using 'catch' becomes trickier. The
-- intuitive behaviour is that each 'catch' shrinks the variant in the left
-- side of my 'MonadError', but this is therefore type-changing: as we can only
-- 'throwError' and 'catchError' with a 'MonadError' type, this is impossible!
--
-- To get round this problem, we have to specialise to 'ExceptT', which allows
-- us to map over the error type and change it as we go. If the error we catch
-- is the one in the variant that we want to handle, we pluck it out and deal
-- with it. Otherwise, we "re-throw" the variant minus the one we've handled.
catchFM :: forall x e e' f m a. ()
  => Monad m
  => CatchF x e e'
  => (f x -> ExceptT (VariantF f e') m a)
  -> ExceptT (VariantF f e ) m a
  -> ExceptT (VariantF f e') m a
catchFM recover xs = mapExceptT (>>= go) xs
  where
    go = \case
      Right success -> pure (Right success)
      Left  failure -> case catchF @x failure of
        Right hit  -> runExceptT (recover hit)
        Left  miss -> pure (Left miss)

-- | Just the same as 'catchFM', but specialised for our plain 'Variant' and
-- sounding much less like a radio station.
catchM :: forall x e e' m a. ()
  => Monad m
  => Catch x e e'
  => (x -> ExceptT (Variant e') m a)
  -> ExceptT (Variant e ) m a
  -> ExceptT (Variant e') m a
catchM recover xs
  = catchFM (recover . runIdentity) xs

-- | Same as 'catchFM' except the error is not removed from the type.
-- This is useful for writing recursive computations or computations that
-- rethrow the same error type.
snatchFM
  :: forall x e f m a. ()
  => Monad m
  => e `CouldBe` x
  => (f x -> ExceptT (VariantF f e) m a)
  -> ExceptT (VariantF f e) m a
  -> ExceptT (VariantF f e) m a
snatchFM recover xs = mapExceptT (>>= go) xs
  where
    go = \case
      Right success -> pure (Right success)
      Left  failure -> case snatchF @_ @_ @x failure of
        Right hit  -> runExceptT (recover hit)
        Left  miss -> pure (Left miss)


-- | Same as 'catchM' except the error is not removed from the type.
-- This is useful for writing recursive computations or computations that
-- rethrow the same error type.
snatchM :: forall x e m a. ()
  => Monad m
  => e `CouldBe` x
  => (x -> ExceptT (Variant e) m a)
  -> ExceptT (Variant e) m a
  -> ExceptT (Variant e) m a
snatchM recover xs = snatchFM (recover . runIdentity) xs

-- | Throw an error into a variant 'MonadError' context. Note that this /isn't/
-- type-changing, so this can work for any 'MonadError', rather than just
-- 'ExceptT'.
throwFM :: forall x e f m a. ()
  => MonadError (VariantF f e) m
  => e `CouldBe` x
  => f x
  -> m a
throwFM = throwError . throwF

-- | Same as 'throwFM', but without the @f@ context. Given a value of some type
-- within a 'Variant' within a 'MonadError' context, "throw" the error.
throwM :: forall x e m a. ()
  => MonadError (Variant e) m
  => e `CouldBe` x
  => x
  -> m a
throwM = throwFM . Identity

-- | Add 'ExceptT (Variant '[])' to the monad transformer stack.
runOops :: ()
  => Monad m
  => ExceptT (Variant '[]) m a
  -> m a
runOops f = either (absurd . preposterous) pure =<< runExceptT f

-- | Convert an 'ExceptT (Variant '[])' expression to an 'ExceptT Void' expression
runOops0 :: forall m a. Monad m => ExceptT (Variant '[]) m a -> ExceptT Void m a
runOops0 = mapExceptT (fmap (first (absurd . preposterous)))

-- | Convert an ExceptT (Variant '[x]) expression to an 'ExceptT x' expression
runOops1 :: forall x m a. Monad m => ExceptT (Variant '[x]) m a -> ExceptT x m a
runOops1 = mapExceptT (fmap (first DV.toEithers))

-- | Suspend the 'ExceptT` monad transformer from the top of the stack so that the
-- stack can be manipulated without the 'ExceptT` layer.
suspendM :: forall x m a n b. ()
  => (m (Either x a) -> n (Either x b))
  -> ExceptT x m a
  -> ExceptT x n b
suspendM f = ExceptT . f . runExceptT

-- | Catch the specified exception and return the caught value as 'Left'.  If no
-- value was caught, then return the returned value in 'Right'.
catchAsLeftM :: forall x e m a. ()
  => Monad m
  => ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m (Either x a)
catchAsLeftM = catchM @x (pure . Left) . fmap Right

-- | Catch the specified exception and return the caught value as 'Left'.  If no
-- value was caught, then return the returned value in 'Right'.
catchAsNothingM :: forall x e m a. ()
  => Monad m
  => ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m (Maybe a)
catchAsNothingM = catchM @x (pure . (const Nothing)) . fmap Just

-- | Catch the specified exception.  If that exception is caught, exit the program.
catchAndExitFailureM :: forall x e m a. ()
  => MonadIO m
  => ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m a
catchAndExitFailureM = catchM @x (const (liftIO IO.exitFailure))

-- | When the expression of type 'Either x a' evaluates to 'Left x', throw the 'x',
-- otherwise return 'a'.
throwLeftM :: forall x e m a. ()
  => MonadError (Variant e) m
  => CouldBeF e x
  => Monad m
  => Either x a
  -> m a
throwLeftM = either throwM pure

-- | When the expression of type 'Maybe a' evaluates to 'Nothing', throw '()',
-- otherwise return 'a'.
throwNothingM :: ()
  => MonadError (Variant e) m
  => CouldBeF e ()
  => Monad m
  => Maybe a
  -> m a
throwNothingM = throwNothingAsM ()

-- | When the expression of type 'Maybe a' evaluates to 'Nothing', throw the specified value,
-- otherwise return 'a'.
throwNothingAsM :: forall e es m a. ()
  => MonadError (Variant es) m
  => CouldBe es e
  => e
  -> Maybe a
  -> m a
throwNothingAsM e = maybe (throwM e) pure

-- | When the expression of type 'm (Either x a)' evaluates to 'pure (Left x)', throw the 'x',
-- otherwise return 'a'.
throwPureLeftM :: forall x e m a. ()
  => MonadError (Variant e) m
  => CouldBeF e x
  => m (Either x a)
  -> m a
throwPureLeftM f = f >>= throwLeftM

-- | When the expression of type 'Maybe a' evaluates to 'Nothing', throw '()',
-- otherwise return 'a'.
throwPureNothingM :: ()
  => MonadError (Variant e) m
  => CouldBeF e ()
  => Monad m
  => m (Maybe a)
  -> m a
throwPureNothingM f = f >>= throwNothingM

-- | When the expression of type 'Maybe a' evaluates to 'Nothing', throw the specified value,
-- otherwise return 'a'.
throwPureNothingAsM :: forall e es m a. ()
  => MonadError (Variant es) m
  => CouldBe es e
  => e
  -> m (Maybe a)
  -> m a
throwPureNothingAsM e f = f >>= throwNothingAsM e

leftM :: forall x m a. ()
  => Monad m
  => (x -> m a)
  -> m (Either x a)
  -> m a
leftM g f = f >>= either g pure

nothingM :: forall m a. ()
  => Monad m
  => m a
  -> m (Maybe a)
  -> m a
nothingM g f = f >>= maybe g pure

-- | Catch the specified exception and return it instead.
-- The evaluated computation must return the same type that is being caught.
recoverM :: forall x e m a. ()
  => Monad m
  => (x -> a)
  -> ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m a
recoverM g f = f & catchM (pure . g)

-- | Catch the specified exception and return it instead.  The evaluated computation
-- must return `Void` (ie. it never returns)
recoverOrVoidM :: forall x e m. ()
  => Monad m
  => ExceptT (Variant (x : e)) m Void
  -> ExceptT (Variant e) m x
recoverOrVoidM f = either pure absurd =<< (fmap Right f & catchM @x (pure . Left))
