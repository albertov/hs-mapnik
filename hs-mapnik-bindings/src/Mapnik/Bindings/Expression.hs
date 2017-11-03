
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Expression (parse) where

import           Mapnik.Bindings
import           Control.Exception (try)
import           Control.Monad ((<=<))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/expression.hpp>"

C.using "namespace mapnik"

-- * Expression


foreign import ccall "&hs_mapnik_destroy_Expression" destroyExpression :: FinalizerPtr Expression

unsafeNew :: (Ptr (Ptr Expression) -> IO ()) -> IO Expression
unsafeNew = fmap Expression . newForeignPtr destroyExpression <=< C.withPtr_

parse :: Text -> Either String Expression
parse (encodeUtf8 -> s) =
  unsafePerformIO $ fmap showExc $ try $ unsafeNew $ \p ->
    [C.catchBlock|*$(expression_ptr **p) = new expression_ptr(parse_expression(std::string($bs-ptr:s, $bs-len:s)));|]
  where
    showExc = either (Left . show @C.CppException) Right
