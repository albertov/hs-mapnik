{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Expression (
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
C.include "<mapnik/expression.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "<mapnik/expression_evaluator.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"

-- * Expression


foreign import ccall "&hs_mapnik_destroy_Expression" destroyExpression :: FinalizerPtr Expression

unsafeNew :: (Ptr (Ptr Expression) -> IO ()) -> IO Expression
unsafeNew = mkUnsafeNew Expression destroyExpression

unsafeNewMaybe :: (Ptr (Ptr Expression) -> IO ()) -> IO (Maybe Expression)
unsafeNewMaybe = mkUnsafeNewMaybe Expression destroyExpression

parse :: Text -> Either String Expression
parse (encodeUtf8 -> s) =
  unsafePerformIO $ fmap showExc $ try $ unsafeNew $ \p ->
    [C.catchBlock|*$(expression_ptr **p) = new expression_ptr(parse_expression(std::string($bs-ptr:s, $bs-len:s)));|]
  where
    showExc = either (Left . show @C.CppException) Right

toText :: Expression -> Text
toText expr = unsafePerformIO $ newText "Expression.toText" $ \(p,len) ->
  [CU.block|void {
  std::string s = to_expression_string(**$fptr-ptr:(expression_ptr *expr));
  mallocedString(s, $(char **p), $(int *len));
  }|]
