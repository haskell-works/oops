{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Variant.Gen () where

import Data.Variant
import Test.QuickCheck.Arbitrary (Arbitrary (..))

instance (EithersF f xs nested, Arbitrary nested) => Arbitrary (VariantF f xs) where
  arbitrary = fmap fromEithersF arbitrary
