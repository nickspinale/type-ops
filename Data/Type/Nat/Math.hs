{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Nat.Math
    ( type (/)
    , type (%)
    , QuotRem
    ) where

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

type family (m :: Nat) / (n :: Nat) where
    m / n = If (CmpNat m n == LT) 0 ((m - n) / n)

type family (m :: Nat) % (n :: Nat) where
    m % n = If (CmpNat m n == LT) m ((m - n) % n)

class QuotRem (up   :: Nat)
              (down :: Nat)
              (quot :: Nat)
              (rem  :: Nat) | up down -> quot rem

instance ( QuotRem up' down quot' rem
         , up ~ (up' + down)
         , quot ~ (quot' + 1)
         ) => QuotRem up down quot rem
