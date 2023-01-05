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
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Control.Monad.Oops.Classic
  ( -- * MTL/transformer utilities
    catchFM,
    catchM,

    snatchFM,
    snatchM,

    Oops.throwFM,
    Oops.throwM,

    Oops.runOops,
    Oops.suspendM,

    Oops.catchAsLeftM,
    Oops.catchAndExitFailureM,

    Oops.throwLeftM,
    Oops.throwNothingM,
    Oops.throwNothingAsM,

    Oops.recoverM,
    Oops.recoverOrVoidM,

    DV.CouldBeF (..),
    DV.CouldBe  (..),
    DV.CouldBeAnyOfF,
    DV.CouldBeAnyOf,
    DV.Variant,
    DV.VariantF(..),

  ) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT(ExceptT))
import Data.Variant ( Catch, CatchF, CouldBe, Variant, VariantF )

import qualified Data.Variant       as DV
import qualified Control.Monad.Oops as Oops

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
  => ExceptT (VariantF f e ) m a
  -> (f x -> ExceptT (VariantF f e') m a)
  -> ExceptT (VariantF f e') m a
catchFM = flip Oops.catchFM

-- | Just the same as 'catchFM', but specialised for our plain 'Variant' and
-- sounding much less like a radio station.
catchM :: forall x e e' m a. ()
  => Monad m
  => Catch x e e'
  => ExceptT (Variant e ) m a
  -> (x -> ExceptT (Variant e') m a)
  -> ExceptT (Variant e') m a
catchM = flip Oops.catchM

-- | Same as 'catchFM' except the error is not removed from the type.
-- This is useful for writing recursive computations or computations that
-- rethrow the same error type.
snatchFM
  :: forall x e f m a. ()
  => Monad m
  => e `CouldBe` x
  => ExceptT (VariantF f e) m a
  -> (f x -> ExceptT (VariantF f e) m a)
  -> ExceptT (VariantF f e) m a
snatchFM = flip Oops.snatchFM

-- | Same as 'catchM' except the error is not removed from the type.
-- This is useful for writing recursive computations or computations that
-- rethrow the same error type.
snatchM :: forall x e m a. ()
  => Monad m
  => e `CouldBe` x
  => ExceptT (Variant e) m a
  -> (x -> ExceptT (Variant e) m a)
  -> ExceptT (Variant e) m a
snatchM = flip Oops.snatchM
