{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.Typeable.Compat
    ( Typeable, typeOf
    , cast, gcast
    , TypeRep, showsTypeRep
    , TyCon, tyConString, tyConPackage, tyConModule, tyConName
    , mkTyCon3, mkTyConApp, mkAppTy, mkFunTy
    , splitTyConApp, funResultTy, typeRepTyCon, typeRepArgs
    , typeRep
    , Proxy(..)
    ) where

import Data.Typeable

#if !MIN_VERSION_base(4,7,0)
import Data.Proxy.Compat

typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
#endif
