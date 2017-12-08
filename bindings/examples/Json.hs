import           Mapnik.Bindings
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as LBS
import           System.Environment (getArgs)

main :: IO ()
main = do
  registerDefaults
  [arg] <- getArgs
  LBS.putStr . encodeToLazyText =<< fromMapnik =<< fromXmlFile arg
