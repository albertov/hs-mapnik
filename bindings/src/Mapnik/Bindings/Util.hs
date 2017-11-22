{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Mapnik.Bindings.Util where

import qualified Mapnik.Bindings.Cpp as C

import           Control.Monad ((<=<))
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Exception.Lifted
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr)
import           Foreign.Storable (Storable)
import           Foreign.StablePtr
import           Foreign.C.String (CString)


newText :: MonadBaseControl IO m
  => String -> ((Ptr CString, Ptr C.CInt) -> m ()) -> m Text
newText ctx = decodeUtf8Ctx ctx <=< newByteString

newTextMaybe :: MonadBaseControl IO m
  => String -> ((Ptr CString, Ptr C.CInt) -> m ()) -> m (Maybe Text)
newTextMaybe msg = mapM (decodeUtf8Ctx msg) <=< newByteStringMaybe


newByteString :: MonadBaseControl IO m
  => ((Ptr CString, Ptr C.CInt) -> m ()) -> m ByteString
newByteString =
  maybe (throwIO (userError "nullPtr")) return <=< newByteStringMaybe

newByteStringMaybe :: MonadBaseControl IO m
  => ((Ptr CString, Ptr C.CInt) -> m ()) -> m (Maybe ByteString)
newByteStringMaybe fun = do
  (ptr,len) <- C.withPtrs_ fun
  if ptr == nullPtr then return Nothing else
    Just <$> liftBase (unsafePackMallocCStringLen (ptr, fromIntegral len))

mkUnsafeNew
  :: MonadBaseControl IO m
  => (ForeignPtr a -> c) -> FinalizerPtr a -> (Ptr (Ptr a) -> m ()) -> m c
mkUnsafeNew a b = maybe (throwIO (userError "nullPtr")) return <=< mkUnsafeNewMaybe a b

mkUnsafeNewMaybe
  :: MonadBaseControl IO m
  => (ForeignPtr a -> c) -> FinalizerPtr a -> (Ptr (Ptr a) -> m ()) -> m (Maybe c)
mkUnsafeNewMaybe ctor dtor fun = do
  ptr <- C.withPtr_ fun
  if ptr == nullPtr then return Nothing else
    Just . ctor <$> liftBase (newForeignPtr dtor ptr)

newMaybe :: (MonadBaseControl IO m, Storable a)
  => ((Ptr C.CInt, Ptr a) -> m ()) -> m (Maybe a)
newMaybe fun = do
  (has, p) <- C.withPtrs_ fun
  return $ if has == 1 then Just p else Nothing

decodeUtf8Keys :: MonadBase IO m => String -> [(ByteString, a)] -> m [(Text, a)]
decodeUtf8Keys msg = mapM (\(k,v) -> (,v) <$> decodeUtf8Ctx msg k)


decodeUtf8Ctx :: MonadBase IO m => String -> ByteString -> m Text
decodeUtf8Ctx msg k' =
  case decodeUtf8' k' of
    Right  k -> return $! k
    Left   e -> throwIO (userError (msg ++ ": Error when decoding " ++ show k' ++ ": " ++ show e))

catchingExceptions :: IO () -> IO (Ptr ())
catchingExceptions act = do
  res <- try act
  case res of
    Right () -> return nullPtr
    Left  (e::SomeException) ->
      castStablePtrToPtr <$> newStablePtr e
