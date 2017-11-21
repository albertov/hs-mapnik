{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Mapnik.Bindings.Cpp (
  block
, safeBlock
, catchBlock
, exp
, withPtr_
, withPtrs_
, module Export
) where
import           Language.C.Inline (WithPtrs(WithPtrsPtrs))
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Language.C.Inline.Unsafe as CU
import           Language.C.Inline.Cpp as Export hiding (block, exp, withPtr_, withPtrs_)

import           Control.Monad.Base (liftBase)
import           Control.Monad.Trans.Control
import           Data.List (isInfixOf)
import           Language.Haskell.TH (Q, Exp)
import           Language.Haskell.TH.Quote
import           Foreign.Ptr
import           Foreign.Storable
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

catchBlock :: QuasiQuoter
catchBlock = liftQQ $ \s -> [e| liftBase $(quoteExp C.catchBlock s) |]

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
