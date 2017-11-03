import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Process
import System.IO
import System.Environment (lookupEnv)
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
 configureWithMapnikConfig lbi flags

configureWithMapnikConfig lbi flags = do
  myExtraLibs    <- getFlagValues 'l' <$> mapnikConfig ["--libs"]
  myExtraLibDirs <- getFlagValues 'L' <$> mapnikConfig ["--ldflags"]
  myIncludeDirs  <- getFlagValues 'I' <$> mapnikConfig ["--includes", "--dep-includes"]
  myCcOptions    <- words <$> mapnikConfig ["--defines", "--cxxflags"]
  myLdOptions    <- words <$> mapnikConfig ["--ldflags"]
  mapnikInputPluginDir <- (escapeWinPathSep . head . words) <$>
    mapnikConfig ["--input-plugins"]
  mapnikFontDir <- (escapeWinPathSep . head . words) <$> (mapnikConfig ["--fonts"])
  --error (show [ myIncludeDirs, myExtraLibDirs])
  let updBinfo bi = bi { extraLibDirs = extraLibDirs bi ++ myExtraLibDirs
                       , extraLibs    = extraLibs    bi ++ myExtraLibs
                       , includeDirs  = includeDirs  bi ++ myIncludeDirs
                       , ccOptions    = ccOptions    bi ++ myCcOptions
                       , ldOptions    = ldOptions    bi ++ myLdOptions
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

getFlagValues f = nub
                . map (\(_:_:v) -> v)
                . filter (\(_:f':_) -> f==f')
                . words

escapeWinPathSep = concatMap go
  where go '\\' = "\\\\"
        go x   = [x]

rstrip c = reverse . dropWhile (==c) . reverse

configProg isShellScript progName envName args = do
  mCmd <- lookupEnv envName
  cmd <- maybe (liftM (rstrip '\n') (getOutput "sh" ["-c", "which " ++ progName])) return mCmd
  rstrip '\n' <$> getOutput (if isShellScript then "sh"       else cmd)
                            (if isShellScript then (cmd:args) else args)

mapnikConfig = configProg True "mapnik-config" "MAPNIK_CONFIG"
gdalConfig = configProg True "gdal-config" "GDAL_CONFIG"
icuConfig = configProg True "icu-config" "ICU_CONFIG"
