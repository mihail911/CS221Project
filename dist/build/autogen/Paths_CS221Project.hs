module Paths_CS221Project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/MTGAP/Library/Haskell/ghc-7.4.2/lib/CS221Project-1.0/bin"
libdir     = "/Users/MTGAP/Library/Haskell/ghc-7.4.2/lib/CS221Project-1.0/lib"
datadir    = "/Users/MTGAP/Library/Haskell/ghc-7.4.2/lib/CS221Project-1.0/share"
libexecdir = "/Users/MTGAP/Library/Haskell/ghc-7.4.2/lib/CS221Project-1.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "CS221Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CS221Project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CS221Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CS221Project_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
