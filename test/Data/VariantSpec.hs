{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.VariantSpec (spec) where

import Data.Variant.Gen ()
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog ((===), forAll, property)
import Test.Hspec (describe, it, Spec)

import qualified Data.Variant as DV
import qualified Hedgehog.Gen.QuickCheck as G

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Data.VariantSpec" $ do
  it "VariantF" $ require $ property $ do
    x <-forAll $ G.arbitrary @(DV.VariantF Maybe '[Int, String])

    DV.fromEithersF (DV.toEithersF x) === x
  it "Variant" $ require $ property $ do
    x <-forAll $ G.arbitrary @(DV.Variant '[Int, String, Bool])

    DV.fromEithersF (DV.toEithersF x) === x
