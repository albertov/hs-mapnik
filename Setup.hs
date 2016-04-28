import Control.Monad
import Data.Maybe
import System.Process
import System.IO
import System.Directory (makeAbsolute)
import System.Exit
import Data.List
import Distribution.Simple
import Distribution.Simple.Setup (configConfigurationsFlags)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks simpleUserHooks {confHook = mapnikConf}

mapnikConf (pkg0, pbi) flags = do
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 configureWithMapnikConfig lbi

configureWithMapnikConfig lbi = do
  mapnikInclude <- mapM makeAbsolute =<< liftM (getFlagValues 'I')
    (getOutput "mapnik-config" ["--includes", "--dep-includes"])
  mapnikLibDirs <- mapM makeAbsolute =<< liftM (getFlagValues 'L')
    (getOutput "mapnik-config" ["--libs", "--dep-libs", "--ldflags"])
  mapnikLibs    <- liftM (getFlagValues 'l')
    (getOutput "mapnik-config" ["--libs", "--dep-libs", "--ldflags"])
  mapnikCcOptions <- liftM words $
    (getOutput "mapnik-config" ["--defines", "--cxxflags"])
  mapnikLdOptions <- liftM (filter (\('-':x:_) -> x/='L') . words)
    (getOutput "mapnik-config" ["--ldflags"])
  mapnikInputPluginDir <- liftM (head . words)
    (getOutput "mapnik-config" ["--input-plugins"])
  mapnikFontDir <- liftM (head . words) $
    (getOutput "mapnik-config" ["--fonts"])
  --error (show [ mapnikInclude, mapnikLibDirs])
  let updBinfo bi = bi { extraLibDirs = extraLibDirs bi ++ mapnikLibDirs
                       , extraLibs    = mapnikLibs      ++ extraLibs bi
                       , includeDirs  = includeDirs  bi ++ mapnikInclude
                       , ccOptions    = ccOptions    bi ++ mapnikCcOptions
                       , ldOptions    = ldOptions    bi ++ mapnikLdOptions
                       , cppOptions   = cppOptions   bi ++ mapnikCppOptions
                       }
      mapnikCppOptions =
        [ "-DDEFAULT_FONT_DIR=\""         ++ mapnikFontDir ++ "\""
        , "-DDEFAULT_INPUT_PLUGIN_DIR=\"" ++ mapnikInputPluginDir ++ "\""
        ]
      updLib lib = lib { libBuildInfo  = updBinfo (libBuildInfo lib)}
      updTs  ts  = ts  { testBuildInfo = updBinfo (testBuildInfo ts)}
      updBm  bm  = bm  { benchmarkBuildInfo = updBinfo (benchmarkBuildInfo bm)}
      updExe ex  = ex  { buildInfo     = updBinfo (buildInfo ex)}
      updLpd lpd = lpd { library       = fmap updLib (library lpd)
                       , testSuites    = map updTs (testSuites lpd)
                       , benchmarks    = map updBm (benchmarks lpd)
                       , executables   = map updExe (executables lpd)
                       }
  let lbi' = lbi { localPkgDescr = updLpd (localPkgDescr lbi) }
  return lbi'

getOutput s a = readProcess s a ""

getFlagValues f = map (\(_:_:v) -> v)
                . filter (\(_:f':_) -> f==f')
                . words
