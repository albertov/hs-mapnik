{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Rule (
  Rule
, unsafeNew
, create
, appendSymbolizer
, getSymbolizers
, getName
, setName
, getFilter
, setFilter
, getMinScale
, setMinScale
, getMaxScale
, setMaxScale
, setElse
, hasElseFilter
, setAlso
, hasAlsoFilter
) where

import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Symbolizer as Symbolizer
import qualified Mapnik.Bindings.Cpp as C
import           Data.IORef
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Storable (poke)
import           Foreign.Ptr (Ptr)

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/rule.hpp>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"

--
-- * Rule


foreign import ccall "&hs_mapnik_destroy_Rule" destroyRule :: FinalizerPtr Rule

unsafeNew :: (Ptr (Ptr Rule) -> IO ()) -> IO Rule
unsafeNew = mkUnsafeNew Rule destroyRule

create :: IO Rule
create  = unsafeNew $ \p -> [C.block|void {*$(rule** p) = new rule();}|]

setName :: Rule -> Text -> IO ()
setName l (encodeUtf8 -> s) =
  [C.block|void { $fptr-ptr:(rule *l)->set_name(std::string($bs-ptr:s, $bs-len:s)); }|]

getName :: Rule -> IO Text
getName r = newText "Rule.getName" $ \(p,len) ->
  [C.block|void {
  std::string const &s = $fptr-ptr:(rule *r)->get_name();
  mallocedString(s, $(char **p), $(int *len));
  }|]

getMinScale :: Rule -> IO Double
getMinScale r = realToFrac <$> [C.exp|double { $fptr-ptr:(rule *r)->get_min_scale()}|]

setMinScale :: Rule -> Double -> IO ()
setMinScale r (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(rule *r)->set_min_scale($(double s)); }|]

setMaxScale :: Rule -> Double -> IO ()
setMaxScale r (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(rule *r)->set_max_scale($(double s)); }|]

getMaxScale :: Rule -> IO Double
getMaxScale r = realToFrac <$> [C.exp|double { $fptr-ptr:(rule *r)->get_max_scale()}|]


appendSymbolizer :: Rule -> Symbolizer -> IO ()
appendSymbolizer r s = [C.block| void {
  symbolizer sym(*$fptr-ptr:(symbolizer *s));
  $fptr-ptr:(rule *r)->append(std::move(sym));
  }|]

getSymbolizers :: Rule -> IO [Symbolizer]
getSymbolizers r = do
  symsRef <- newIORef []
  let callback :: Ptr Symbolizer -> IO ()
      callback ptr = do
        sym <- Symbolizer.unsafeNew (flip poke ptr)
        modifyIORef' symsRef (sym:)
  [C.safeBlock|void {
  for (rule::symbolizers::const_iterator it=$fptr-ptr:(rule *r)->begin(); it!=$fptr-ptr:(rule *r)->end(); ++it) {
    $fun:(void (*callback)(symbolizer *))(new symbolizer(*it));
  }
  }|]
  reverse <$> readIORef symsRef

setFilter :: Rule -> Expression -> IO ()
setFilter r s = [C.block| void {
  expression_ptr const& filter(*$fptr-ptr:(expression_ptr *s));
  $fptr-ptr:(rule *r)->set_filter(filter);
  }|]

getFilter :: Rule -> IO (Maybe Expression)
getFilter r = Expression.unsafeNewMaybe $ \p -> [C.block| void {
  rule dfl;
  expression_ptr const& expr = $fptr-ptr:(rule *r)->get_filter();
  std::string filter = mapnik::to_expression_string(*expr);
  std::string default_filter = mapnik::to_expression_string(*dfl.get_filter());
  if (filter != default_filter) {
    *$(expression_ptr **p) = new expression_ptr(expr);
  } else {
    *$(expression_ptr **p) = NULL;
  }
  }|]

hasElseFilter :: Rule -> IO Bool
hasElseFilter r = toEnum . fromIntegral <$> [C.exp|int { $fptr-ptr:(rule *r)->has_else_filter()}|]

setElse :: Rule -> Bool -> IO ()
setElse r (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(rule *r)->set_else($(int q)); }|]

hasAlsoFilter :: Rule -> IO Bool
hasAlsoFilter r = toEnum . fromIntegral <$> [C.exp|int { $fptr-ptr:(rule *r)->has_also_filter()}|]

setAlso :: Rule -> Bool -> IO ()
setAlso r (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(rule *r)->set_also($(int q)); }|]
