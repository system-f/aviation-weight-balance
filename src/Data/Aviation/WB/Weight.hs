{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Weight(
  Weight
, HasWeight(..)
, HasWeights(..)
, SetWeight(..)
, HasWeight0(..)
, kilogramsW
, poundsW
) where

import Control.Category((.))
import Control.Lens(Iso', iso, Lens', Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units(Kilograms(kilograms), Pounds(pounds))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Data.Ratio((%))
import Data.Semigroup(Semigroup((<>)))
import Numeric.Lens(multiplying)
import Prelude(Show, Rational, (+))

newtype Weight =
  Weight
    Rational
  deriving (Eq, Ord, Show)

makeClassy ''Weight

class HasWeights a where
  weights ::
    Traversal'
      a
      Weight

instance HasWeights Weight where
  weights =
    weight

class SetWeight a where
  setWeight ::
    Setter'
      a
      Weight

instance SetWeight Weight where
  setWeight =
    weight

class HasWeight0 a where
  weight0 ::
    Lens'
      a
      (Maybe Weight)

instance Semigroup Weight where
  (<>) =
    mappend

instance Monoid Weight where
  mempty =
    Weight 0
  Weight w1 `mappend` Weight w2 =
    Weight (w1 + w2)

instance Kilograms Weight where
  kilograms =
    multiplying (22046226218 % 10000000000) . pounds

instance Pounds Weight where
  pounds =
    iso
      Weight
      (\(Weight x) -> x)

kilogramsW ::
  Iso'
    Rational
    Weight
kilogramsW =
  kilograms
  
poundsW ::
  Iso'
    Rational
    Weight
poundsW =
  pounds
