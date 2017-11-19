{-# LANGUAGE TupleSections #-}
module Mapnik.Bindings.Util where

import           Control.Monad ((<=<))
import           Control.Exception
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.C.String (CString)
import           Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr)
import           Foreign.Storable (Storable)

import qualified Language.C.Inline.Cpp as C

newText :: String -> ((Ptr CString, Ptr C.CInt) -> IO ()) -> IO Text
newText ctx = decodeUtf8Ctx ctx <=< newByteString

newTextMaybe :: String -> ((Ptr CString, Ptr C.CInt) -> IO ()) -> IO (Maybe Text)
newTextMaybe msg = mapM (decodeUtf8Ctx msg) <=< newByteStringMaybe


newByteString :: ((Ptr CString, Ptr C.CInt) -> IO ()) -> IO ByteString
newByteString =
  maybe (throwIO (userError "nullPtr")) return <=< newByteStringMaybe

newByteStringMaybe :: ((Ptr CString, Ptr C.CInt) -> IO ()) -> IO (Maybe ByteString)
newByteStringMaybe block = do
  (ptr,len) <- C.withPtrs_ block
  if ptr == nullPtr then return Nothing else
    Just <$> unsafePackMallocCStringLen (ptr, fromIntegral len)

mkUnsafeNew
  :: (ForeignPtr a -> c)
  -> FinalizerPtr a -> (Ptr (Ptr a) -> IO ()) -> IO c
mkUnsafeNew a b = maybe (throwIO (userError "nullPtr")) return <=< mkUnsafeNewMaybe a b

mkUnsafeNewMaybe
  :: (ForeignPtr a -> c)
  -> FinalizerPtr a -> (Ptr (Ptr a) -> IO ()) -> IO (Maybe c)
mkUnsafeNewMaybe ctor dtor fun = do
  ptr <- C.withPtr_ fun
  if ptr == nullPtr then return Nothing else 
    Just . ctor <$> newForeignPtr dtor ptr

newMaybe :: Storable a => ((Ptr C.CInt, Ptr a) -> IO ()) -> IO (Maybe a)
newMaybe fun = do
  (has, p) <- C.withPtrs_ fun
  return $ if has == 1 then Just p else Nothing

decodeUtf8Keys :: String -> [(ByteString, a)] -> IO [(Text, a)]
decodeUtf8Keys msg = mapM (\(k,v) -> (,v) <$> decodeUtf8Ctx msg k)


decodeUtf8Ctx :: String -> ByteString -> IO Text
decodeUtf8Ctx msg k' =
  case decodeUtf8' k' of
    Right  k -> return $! k
    Left   e -> throwIO (userError (msg ++ ": Error when decoding " ++ show k' ++ ": " ++ show e))
