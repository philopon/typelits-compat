{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

-- | this module exports:
--
-- Kinds:
--
-- @
-- Nat
-- Symbol
-- @
--
-- Classes:
--
-- @
-- KnownNat n
-- KnownSymbol n
-- @
--
-- Values:
--
-- @
-- natVal :: forall n proxy. KnownNat n => proxy n -> Integer
-- symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
-- @
--
-- types
--
-- @
-- type (<=) x y = (x <=? y) ~ True
--
-- type family m <=? n :: Bool
-- type family m + n :: Nat
-- type family m * n :: Nat
-- type family m * n :: Nat
-- @

module GHC.TypeLits.Compat
    ( -- * Kinds
      Nat, Symbol
      -- * Linking type and value level
    , KnownNat, natVal
    , KnownSymbol, symbolVal
    , type (<=)
    , type (<=?)
    , type (+)
    , type (*)
    , type (^)
    ) where

import GHC.TypeLits

#if __GLASGOW_HASKELL__ < 707
data Proxy k = Proxy

class SingI n => KnownNat (n :: Nat) where
    natSing :: Sing n

instance SingRep n Integer => KnownNat n where
    natSing = sing

natVal :: forall n proxy. KnownNat n => proxy n -> Integer
natVal _ = fromSing (natSing :: Sing n)

class SingI n => KnownSymbol (n :: Symbol) where
    symbolSing :: Sing n

instance SingRep n String => KnownSymbol n where
    symbolSing = sing

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (symbolSing :: Sing n)
#endif
