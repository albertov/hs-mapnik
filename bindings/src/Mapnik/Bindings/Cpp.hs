{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
module Mapnik.Bindings.Cpp (
  block
, safeBlock
, catchBlock
, exp
, withPtr_
, withPtrs_
, module Export
) where
import           Mapnik.Bindings.Types (MapnikError(..))
import           Language.C.Inline (WithPtrs(WithPtrsPtrs))
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           Language.C.Inline.Cpp as Export hiding (block, exp, withPtr_, withPtrs_)

import           Control.Monad.Base (liftBase)
import           Control.Monad.Trans.Control
import           Control.Exception
import           Data.List (isInfixOf)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Foreign
import           Foreign.C
import           Prelude hiding (exp)

block :: QuasiQuoter
block = liftQQ $ \s -> if "$fun:" `isInfixOf` s
  then fail "Found a callback to haskell in an unsafe block"
  else [e| liftBase $(quoteExp CU.block s) |]

exp :: QuasiQuoter
exp = liftQQ $ \s -> if "$fun:" `isInfixOf` s
  then fail "Found a callback to haskell in an unsafe exp"
  else [e| liftBase $(quoteExp CU.exp s) |]

safeBlock :: QuasiQuoter
safeBlock = liftQQ $ \s -> [e| liftBase $(quoteExp C.block s) |]

liftQQ :: (String -> Q Exp) -> QuasiQuoter
liftQQ f = QuasiQuoter
  { quoteExp = f
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  }
  where
    unsupported _ = fail "Unsupported quasiquotation."

withPtrs_ :: (WithPtrs a, MonadBaseControl IO m) => (WithPtrsPtrs a -> m ()) -> m a
withPtrs_ = liftBaseOpDiscard C.withPtrs_

withPtr_ :: (Storable a, MonadBaseControl IO m) => (Ptr a -> m ()) -> m a
withPtr_ = liftBaseOpDiscard C.withPtr_

pattern ExTypeNoException :: CInt
pattern ExTypeNoException = 0

pattern ExTypeStdException :: CInt
pattern ExTypeStdException = 1

pattern ExTypeVariantTypeError :: CInt
pattern ExTypeVariantTypeError = 2

handleForeign :: MonadBaseControl IO m => (Ptr CInt -> Ptr CString -> IO ()) -> m ()
handleForeign cont = liftBase $
  alloca $ \exTypePtr ->
  alloca $ \msgPtrPtr -> do
    poke exTypePtr ExTypeNoException
    cont exTypePtr msgPtrPtr `finally` do
      exType <- peek exTypePtr
      case exType of
        ExTypeNoException -> return ()
        ExTypeStdException -> do
          msgPtr <- peek msgPtrPtr
          errMsg <- peekCString msgPtr
          free msgPtr
          throwIO $ CppStdException errMsg
        _ -> error "Unexpected C++ exception type."

-- | Similar to `C.block`, but C++ exceptions will be caught and rethrown as `ForeignException`s.
-- Unlike `C.block`, the return type can only be @void@ (and doesn't need to be specified), but you can use `C.withPtr_` to extract a result yourself.
--
-- Using this will automatically include @exception@, @cstring@ and @cstdlib@.
catchBlock :: QuasiQuoter
catchBlock = QuasiQuoter
  { quoteExp = \blockStr -> do
      _ <- C.include "<exception>"
      _ <- C.include "<cstring>"
      _ <- C.include "<cstdlib>"
      typePtrVarName <- newName "exTypePtr"
      msgPtrVarName <- newName "msgPtr"
      let inlineCStr = unlines
            [ "void {"
            , "  int* __inline_c_cpp_exception_type__ = $(int* " ++ nameBase typePtrVarName ++ ");"
            , "  char** __inline_c_cpp_error_message__ = $(char** " ++ nameBase msgPtrVarName ++ ");"
            , "  try {"
            , blockStr
            , "  } catch (std::exception &e) {"
            , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeStdException ++ ";"
            , "    size_t whatLen = std::strlen(e.what()) + 1;"
            , "    *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(whatLen));"
            , "    std::memcpy(*__inline_c_cpp_error_message__, e.what(), whatLen);"
            , "  }"
            , "}"
            ]
      [e| handleForeign $ \ $(varP typePtrVarName) $(varP msgPtrVarName) -> $(quoteExp C.block inlineCStr) |]

  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."
