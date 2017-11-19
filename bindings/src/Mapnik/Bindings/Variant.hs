{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
module Mapnik.Bindings.Variant (
  Variant(..)
, VariantPtr (..)
, VariantTypeError(..)
, justOrTypeError
, withV
) where

import           Control.Exception
import           Data.Typeable
import           Foreign.Ptr (Ptr)

class VariantPtr v where
  allocaV :: (Ptr v -> IO a) -> IO a

class Variant v a where
  pokeV :: Ptr v -> a -> IO ()
  peekV :: Ptr v -> IO a

data VariantTypeError = VariantTypeError String
  deriving (Show, Typeable, Exception)

justOrTypeError :: String -> IO (Maybe b) -> IO b
justOrTypeError msg = (maybe (throwIO (VariantTypeError msg)) return =<<)

withV :: (VariantPtr p, Variant p a) => a -> (Ptr p -> IO b) -> IO b
withV v f = allocaV (\p -> pokeV p v >> f p)
