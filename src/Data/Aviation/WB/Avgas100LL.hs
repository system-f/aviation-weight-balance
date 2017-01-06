{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Aviation.WB.Avgas100LL(
  Avgas100LL(..)
) where

import Control.Category((.))
import Control.Lens(Iso', from)
import Data.Aviation.Units.USGallons(usgallons)
import Data.Aviation.Units.Pounds(pounds)
import Data.Aviation.WB.Weight(Weight)
import Data.Aviation.WB.Volume(Volume)
import Numeric.Lens(multiplying)

class Avgas100LL a b | a -> b, b -> where
  avgas100LL ::
    Iso' a b

instance Avgas100LL Volume Weight where
  avgas100LL =
    from usgallons . multiplying 6 . pounds

instance Avgas100LL Weight Volume where
  avgas100LL =
    from avgas100LL
