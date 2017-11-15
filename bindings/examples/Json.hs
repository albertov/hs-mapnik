import Mapnik
import Data.Aeson.Text (encodeToLazyText)
import qualified Mapnik.Bindings.Map as Map
import           Mapnik.Bindings.FromMapnik
import           Mapnik.Bindings.Registry
import qualified Data.Text.Lazy.IO as LBS
import           System.Environment

main = do
  [arg] <- getArgs
  registerDefaults
  m <- Map.create
  Map.loadXmlFile m arg
  LBS.putStr . encodeToLazyText =<< fromMapnik m
