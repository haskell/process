{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- | Miscellaneous information about the system environment, for 'OsString'.
--
-- @since X.Y.Z
module System.Process.Environment.OsString (
    getArgs,
    getEnv,
    getEnvironment,
    ) where

import Data.Coerce (coerce)
#if MIN_VERSION_filepath(1, 5, 0)
import "os-string" System.OsString.Internal.Types (OsString(OsString))
#else
import "filepath" System.OsString.Internal.Types (OsString(OsString))
#endif
#if defined(mingw32_HOST_OS)
import qualified System.Win32.WindowsString.Console as Platform
#else
import qualified System.Posix.Env.PosixString as Platform
#endif

-- | 'System.Environment.getArgs' for 'OsString'.
--
-- @since X.Y.Z
getArgs :: IO [OsString]
getArgs = coerce Platform.getArgs

-- | 'System.Environment.getEnv' for 'OsString'.
--
-- @since X.Y.Z
getEnv :: OsString -> IO (Maybe OsString)
getEnv = coerce Platform.getEnv

-- | 'System.Environment.getEnvironment' for 'OsString'.
--
-- @since X.Y.Z
getEnvironment :: IO [(OsString, OsString)]
getEnvironment = coerce Platform.getEnvironment
