{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Mapnik.Bindings.Variant (
  Variant(..)
, VariantPtr (..)
, VariantTypeError(..)
, justOrTypeError
, withV
) where

import           Control.Exception.Lifted
import           Control.Monad.Base (MonadBase)
import           Data.Typeable
import           Foreign.Ptr (Ptr)


class VariantPtr v where
  type VariantM v :: * -> *
  type VariantM v = IO
  allocaV :: (Ptr v -> VariantM v a) -> VariantM v a

class Variant v a where
  pokeV :: Ptr v -> a -> VariantM v ()
  peekV :: Ptr v -> VariantM v a

newtype VariantTypeError = VariantTypeError String
  deriving (Show, Typeable, Exception)

justOrTypeError :: MonadBase IO m => String -> m (Maybe b) -> m b
justOrTypeError msg = (maybe (throwIO (VariantTypeError msg)) return =<<)

withV :: (Monad (VariantM p), VariantPtr p, Variant p a)
      => a -> (Ptr p -> VariantM p b) -> VariantM p b
withV v f = allocaV (\p -> pokeV p v >> f p)
