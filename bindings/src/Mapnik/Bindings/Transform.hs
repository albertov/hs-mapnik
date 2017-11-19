{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Transform (
  parse
, unsafeNew
, unsafeNewMaybe
, toText
) where

import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Control.Exception (try)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Language.C.Inline.Unsafe as CU

import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/transform_expression.hpp>"
C.include "<mapnik/parse_transform.hpp>"
C.include "<mapnik/transform_processor.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"

-- * Transform


foreign import ccall "&hs_mapnik_destroy_Transform" destroyTransform :: FinalizerPtr Transform

unsafeNew :: (Ptr (Ptr Transform) -> IO ()) -> IO Transform
unsafeNew = mkUnsafeNew Transform destroyTransform

unsafeNewMaybe :: (Ptr (Ptr Transform) -> IO ()) -> IO (Maybe Transform)
unsafeNewMaybe = mkUnsafeNewMaybe Transform destroyTransform

parse :: Text -> Either String Transform
parse (encodeUtf8 -> s) =
  unsafePerformIO $ fmap showExc $ try $ unsafeNew $ \p ->
    [C.catchBlock|*$(transform_type **p) = new transform_type(parse_transform(std::string($bs-ptr:s, $bs-len:s)));|]
  where
    showExc = either (Left . show @C.CppException) Right


toText :: Transform -> Text
toText trans = unsafePerformIO $ newText "Transform.toText" $ \(ptr,len) ->
  [CU.block|void {
  std::string s = transform_processor_type::to_string(**$fptr-ptr:(transform_type *trans));
  mallocedString(s, $(char **ptr), $(int *len));
  }|]
