{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_relations (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/workspaces/hqdmHaskell/relations/.stack-work/install/x86_64-linux/4aecb26cb57d76c9490b44a9cc01f771fefa1de93ce424559d9326fcad137434/9.4.8/bin"
libdir     = "/workspaces/hqdmHaskell/relations/.stack-work/install/x86_64-linux/4aecb26cb57d76c9490b44a9cc01f771fefa1de93ce424559d9326fcad137434/9.4.8/lib/x86_64-linux-ghc-9.4.8/relations-0.1.0.0-65wPvAX6lYJHGTs6i8wv0O-relations"
dynlibdir  = "/workspaces/hqdmHaskell/relations/.stack-work/install/x86_64-linux/4aecb26cb57d76c9490b44a9cc01f771fefa1de93ce424559d9326fcad137434/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/workspaces/hqdmHaskell/relations/.stack-work/install/x86_64-linux/4aecb26cb57d76c9490b44a9cc01f771fefa1de93ce424559d9326fcad137434/9.4.8/share/x86_64-linux-ghc-9.4.8/relations-0.1.0.0"
libexecdir = "/workspaces/hqdmHaskell/relations/.stack-work/install/x86_64-linux/4aecb26cb57d76c9490b44a9cc01f771fefa1de93ce424559d9326fcad137434/9.4.8/libexec/x86_64-linux-ghc-9.4.8/relations-0.1.0.0"
sysconfdir = "/workspaces/hqdmHaskell/relations/.stack-work/install/x86_64-linux/4aecb26cb57d76c9490b44a9cc01f771fefa1de93ce424559d9326fcad137434/9.4.8/etc"

getBinDir     = catchIO (getEnv "relations_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "relations_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "relations_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "relations_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "relations_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "relations_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
