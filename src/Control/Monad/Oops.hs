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
    catchF,
    catch,

    throwF,
    throw,

    snatchF,
    snatch,

    runOops,
    runOops0,
    runOops1,
    suspend,

    catchAsLeft,
    catchAsNothing,
    catchAndExitFailure,

    hoistEither,
    hoistMaybe,

    onLeftThrow,
    onNothingThrow,

    onLeft,
    onNothing,

    recover,
    recoverOrVoid,

    onExceptionThrow,
    onException,

    CouldBeF,
    CouldBe,
    CouldBeAnyOfF,
    CouldBeAnyOf,
    Variant,
    VariantF,

  ) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (mapExceptT, runExceptT)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Variant (Catch, CatchF, CouldBe, CouldBeAnyOf, CouldBeAnyOfF, CouldBeF, Variant, VariantF)
import Data.Void (Void, absurd)

import qualified Control.Monad.Catch as CMC
import qualified Data.Variant        as DV
import qualified System.Exit         as IO

-- | When working in some monadic context, using 'catch' becomes trickier. The
-- intuitive behaviour is that each 'catch' shrinks the variant in the left
-- side of my 'MonadError', but this is therefore type-changing: as we can only
-- 'throwError' and 'catchError' with a 'MonadError' type, this is impossible!
--
-- To get round this problem, we have to specialise to 'ExceptT', which allows
-- us to map over the error type and change it as we go. If the error we catch
-- is the one in the variant that we want to handle, we pluck it out and deal
-- with it. Otherwise, we "re-throw" the variant minus the one we've handled.
catchF :: forall x e e' f m a. ()
  => Monad m
  => CatchF x e e'
  => (f x -> ExceptT (VariantF f e') m a)
  -> ExceptT (VariantF f e ) m a
  -> ExceptT (VariantF f e') m a
catchF h xs = mapExceptT (>>= go) xs
  where
    go = \case
      Right success -> pure (Right success)
      Left  failure -> case DV.catchF @x failure of
        Right hit  -> runExceptT (h hit)
        Left  miss -> pure (Left miss)

-- | Just the same as 'catchF', but specialised for our plain 'Variant' and
-- sounding much less like a radio station.
catch :: forall x e e' m a. ()
  => Monad m
  => Catch x e e'
  => (x -> ExceptT (Variant e') m a)
  -> ExceptT (Variant e ) m a
  -> ExceptT (Variant e') m a
catch h xs
  = catchF (h . runIdentity) xs

-- | Same as 'catchF' except the error is not removed from the type.
-- This is useful for writing recursive computations or computations that
-- rethrow the same error type.
snatchF
  :: forall x e f m a. ()
  => Monad m
  => e `CouldBe` x
  => (f x -> ExceptT (VariantF f e) m a)
  -> ExceptT (VariantF f e) m a
  -> ExceptT (VariantF f e) m a
snatchF h xs = mapExceptT (>>= go) xs
  where
    go = \case
      Right success -> pure (Right success)
      Left  failure -> case DV.snatchF @_ @_ @x failure of
        Right hit  -> runExceptT (h hit)
        Left  miss -> pure (Left miss)


-- | Same as 'catch' except the error is not removed from the type.
-- This is useful for writing recursive computations or computations that
-- rethrow the same error type.
snatch :: forall x e m a. ()
  => Monad m
  => e `CouldBe` x
  => (x -> ExceptT (Variant e) m a)
  -> ExceptT (Variant e) m a
  -> ExceptT (Variant e) m a
snatch h xs = snatchF (h . runIdentity) xs

-- | Throw an error into a variant 'MonadError' context. Note that this /isn't/
-- type-changing, so this can work for any 'MonadError', rather than just
-- 'ExceptT'.
throwF :: forall x e f m a. ()
  => MonadError (VariantF f e) m
  => e `CouldBe` x
  => f x
  -> m a
throwF = throwError . DV.throwF

-- | Same as 'throwF', but without the @f@ context. Given a value of some type
-- within a 'Variant' within a 'MonadError' context, "throw" the error.
throw :: forall x e m a. ()
  => MonadError (Variant e) m
  => e `CouldBe` x
  => x
  -> m a
throw = throwF . Identity

-- | Add 'ExceptT (Variant '[])' to the monad transformer stack.
runOops :: ()
  => Monad m
  => ExceptT (Variant '[]) m a
  -> m a
runOops f = either (absurd . DV.preposterous) pure =<< runExceptT f

-- | Convert an 'ExceptT (Variant '[])' expression to an 'ExceptT Void' expression
runOops0 :: forall m a. Monad m => ExceptT (Variant '[]) m a -> ExceptT Void m a
runOops0 = mapExceptT (fmap (first (absurd . DV.preposterous)))

-- | Convert an ExceptT (Variant '[x]) expression to an 'ExceptT x' expression
runOops1 :: forall x m a. Monad m => ExceptT (Variant '[x]) m a -> ExceptT x m a
runOops1 = mapExceptT (fmap (first DV.toEithers))

-- | Suspend the 'ExceptT` monad transformer from the top of the stack so that the
-- stack can be manipulated without the 'ExceptT` layer.
suspend :: forall x m a n b. ()
  => (m (Either x a) -> n (Either x b))
  -> ExceptT x m a
  -> ExceptT x n b
suspend f = ExceptT . f . runExceptT

-- | Catch the specified exception and return the caught value as 'Left'.  If no
-- value was caught, then return the returned value in 'Right'.
catchAsLeft :: forall x e m a. ()
  => Monad m
  => ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m (Either x a)
catchAsLeft = catch @x (pure . Left) . fmap Right

-- | Catch the specified exception and return 'Nothing'.  If no
-- value was caught, then return the returned value in 'Just'.
catchAsNothing :: forall x e m a. ()
  => Monad m
  => ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m (Maybe a)
catchAsNothing = catch @x (pure . (const Nothing)) . fmap Just

-- | Catch the specified exception.  If that exception is caught, exit the program.
catchAndExitFailure :: forall x e m a. ()
  => MonadIO m
  => ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m a
catchAndExitFailure = catch @x (const (liftIO IO.exitFailure))

-- | When the expression of type 'Either x a' evaluates to 'Left x', throw the 'x',
-- otherwise return 'a'.
hoistEither :: forall x e m a. ()
  => MonadError (Variant e) m
  => CouldBeF e x
  => Monad m
  => Either x a
  -> m a
hoistEither = either throw pure

-- | When the expression of type 'Maybe a' evaluates to 'Nothing', throw the specified value,
-- otherwise return 'a'.
hoistMaybe :: forall e es m a. ()
  => MonadError (Variant es) m
  => CouldBe es e
  => e
  -> Maybe a
  -> m a
hoistMaybe e = maybe (throw e) pure

-- | When the expression of type 'm (Either x a)' evaluates to 'pure (Left x)', throw the 'x',
-- otherwise return 'a'.
onLeftThrow :: forall x e m a. ()
  => MonadError (Variant e) m
  => CouldBeF e x
  => m (Either x a)
  -> m a
onLeftThrow f = f >>= hoistEither

-- | When the expression of type 'Maybe a' evaluates to 'Nothing', throw the specified value,
-- otherwise return 'a'.
onNothingThrow :: forall e es m a. ()
  => MonadError (Variant es) m
  => CouldBe es e
  => e
  -> m (Maybe a)
  -> m a
onNothingThrow e f = f >>= hoistMaybe e

onLeft :: forall x m a. ()
  => Monad m
  => (x -> m a)
  -> m (Either x a)
  -> m a
onLeft g f = f >>= either g pure

onNothing :: forall m a. ()
  => Monad m
  => m a
  -> m (Maybe a)
  -> m a
onNothing g f = f >>= maybe g pure

-- | Catch the specified exception and return it instead.
-- The evaluated computation must return the same type that is being caught.
recover :: forall x e m a. ()
  => Monad m
  => (x -> a)
  -> ExceptT (Variant (x : e)) m a
  -> ExceptT (Variant e) m a
recover g f = f & catch (pure . g)

-- | Catch the specified exception and return it instead.  The evaluated computation
-- must return `Void` (ie. it never returns)
recoverOrVoid :: forall x e m. ()
  => Monad m
  => ExceptT (Variant (x : e)) m Void
  -> ExceptT (Variant e) m x
recoverOrVoid f = either pure absurd =<< (fmap Right f & catch @x (pure . Left))

-- | Catch an exception of the specified type 'x' and throw it as an error
onExceptionThrow :: forall x e m a. ()
  => CMC.MonadCatch m
  => CMC.Exception x
  => MonadError (Variant e) m
  => CouldBeF e x
  => m a
  -> m a
onExceptionThrow = onException @x throw

-- | Catch an exception of the specified type 'x' and call the the handler 'h'
onException :: forall x m a. ()
  => CMC.MonadCatch m
  => CMC.Exception x
  => (x -> m a)
  -> m a
  -> m a
onException h f = either h pure =<< CMC.try f
