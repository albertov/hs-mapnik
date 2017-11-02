module Mapnik.Internal.Util where

import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.ByteString (ByteString)
import           Foreign.Ptr (Ptr)
import           Foreign.C.String (CString)

import qualified Language.C.Inline.Cpp as C

newByteString :: ((Ptr CString, Ptr C.CInt) -> IO ()) -> IO ByteString
newByteString block = do
  (ptr,len) <- C.withPtrs_ block
  unsafePackMallocCStringLen (ptr, fromIntegral len)
