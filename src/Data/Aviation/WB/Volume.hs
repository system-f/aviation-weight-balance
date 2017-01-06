{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.WB.Volume(
  Volume
, HasVolume(..)
, HasVolumes(..)
, SetVolume(..)
, HasVolume0(..)
, usgallonsV
, litresV
, imperialgallonsV
) where

import Control.Category((.))
import Control.Lens(makeClassy, Traversal', Setter', Lens', Iso', iso)
import Data.Aviation.Units.ImperialGallons(ImperialGallons(imperialgallons))
import Data.Aviation.Units.USGallons(USGallons(usgallons))
import Data.Aviation.Units.Litres(Litres(litres))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Data.Ratio((%))
import Data.Semigroup(Semigroup((<>)))
import Numeric.Lens(multiplying, dividing)
import Prelude(Show, Rational, (+), (*))

newtype Volume =
  Volume
    Rational
  deriving (Eq, Ord, Show)

makeClassy ''Volume

class HasVolumes a where
  volumes ::
    Traversal'
      a
      Volume

instance HasVolumes Volume where
  volumes =
    volume

class SetVolume a where
  setVolume ::
    Setter'
      a
      Volume

instance SetVolume Volume where
  setVolume =
    volume

class HasVolume0 a where
  volume0 ::
    Lens'
      a
      (Maybe Volume)

instance Semigroup Volume where
  (<>) =
    mappend

instance Monoid Volume where
  mempty =
    Volume 0
  Volume w1 `mappend` Volume w2 =
    Volume (w1 + w2)

instance USGallons Volume where
  usgallons =
    iso
      Volume
      (\(Volume x) -> x)

instance Litres Volume where
  litres =
    dividing ((254 * 254 * 254 * 231) % 1000000000) . usgallons

instance ImperialGallons Volume where
  imperialgallons =
    multiplying (454609 % 100000) . litres

usgallonsV ::
  Iso'
    Rational
    Volume
usgallonsV =
  usgallons

litresV ::
  Iso'
    Rational
    Volume
litresV =
  litres

imperialgallonsV ::
  Iso'
    Rational
    Volume
imperialgallonsV =
  imperialgallons
