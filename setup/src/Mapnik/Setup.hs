module Mapnik.Setup (mapnikConfigHook, defaultMapnikMainWith, defaultMapnikMain) where

import Control.Monad
import System.Process
import System.Environment (lookupEnv)
import Data.List
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Setup (configConfigurationsFlags)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

defaultMapnikMain :: IO ()
defaultMapnikMain  = defaultMapnikMainWith id

defaultMapnikMainWith f = defaultMainWithHooks simpleUserHooks {confHook = mapnikConfigHook f}

mapnikConfigHook f (pkg0, pbi) flags = do
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 configureWithMapnikConfig f lbi flags

configureWithMapnikConfig f lbi flags = do
  myExtraLibs     <- getFlagValues 'l' <$> mapnikConfig ["--libs"]
  icuExtraLibs    <- getFlagValues 'l' <$> icuConfig ["--ldflags"]
  icuExtraLibDirs <- getFlagValues 'L' <$> icuConfig ["--ldflags"]
  myExtraLibDirs  <- getFlagValues 'L' <$> mapnikConfig ["--ldflags"]
  myIncludeDirs   <- getFlagValues 'I' <$> mapnikConfig ["--includes", "--dep-includes"]
  myCppOptions    <- words <$> mapnikConfig ["--defines"]
  -- Strip optimization flags to avoid -O3 on development. We can still set it with
  -- "cc-options"
  myCcOptions    <- filter (not . isPrefixOf "-O") . words
                <$> mapnikConfig ["--defines", "--cxxflags"]
  myLdOptions    <- words <$> mapnikConfig ["--ldflags"]
  icuLdOptions    <- words <$> icuConfig ["--ldflags"]
  mapnikInputPluginDir <- (escapeWinPathSep . head . words)
                      <$> mapnikConfig ["--input-plugins"]
  mapnikFontDir <- escapeWinPathSep . head . words <$> mapnikConfig ["--fonts"]
  let updBinfo bi = f $ bi { extraLibDirs = extraLibDirs bi ++ myExtraLibDirs ++ icuExtraLibDirs
                           , extraLibs    = extraLibs    bi ++ myExtraLibs ++ icuExtraLibs
                           , includeDirs  = includeDirs  bi ++ myIncludeDirs
                           , ccOptions    = ccOptions    bi ++ myCcOptions
                           , ldOptions    = ldOptions    bi ++ myLdOptions ++ icuLdOptions
                           , cppOptions   = cppOptions   bi ++ mapnikCppOptions
                           , options      = options      bi ++ myGhcOptions
                           }

      -- | Work around https://github.com/haskell/cabal/issues/4435
      -- | We *must* compile auto-generated code with, at least, the correct
      -- | BIGINT flag or else obscure linking errors will ensue
      myGhcOptions = [(GHC, "-Wall":map ("-optc"++) myCppOptions)]

      mapnikCppOptions =
        [ "-DDEFAULT_FONT_DIR=\""         ++ mapnikFontDir ++ "\""
        , "-DDEFAULT_INPUT_PLUGIN_DIR=\"" ++ mapnikInputPluginDir ++ "\""
        ] ++ myCppOptions
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

getFlagValues f = nub
                . map (\(_:_:v) -> v)
                . filter (\(_:f':_) -> f==f')
                . words

escapeWinPathSep = concatMap go
  where go '\\' = "\\\\"
        go x   = [x]

rstrip c = reverse . dropWhile (==c) . reverse

configProg isShellScript progName envName args = do
  cmd <- fromMaybe progName <$> lookupEnv envName
  rstrip '\n' <$> getOutput cmd args

mapnikConfig = configProg True "mapnik-config" "MAPNIK_CONFIG"
gdalConfig = configProg True "gdal-config" "GDAL_CONFIG"
icuConfig = configProg True "icu-config" "ICU_CONFIG"
