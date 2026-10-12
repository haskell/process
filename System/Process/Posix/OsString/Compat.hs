{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- Select the appropriate imports depending onf the version of the filepath
-- package.
#if MIN_VERSION_filepath(1,5,0)
#define OS_STRING_PACKAGE "os-string"
#define BYTESTRING_MODULE_PREFIX System.OsString
#else
#define OS_STRING_PACKAGE "filepath"
#define BYTESTRING_MODULE_PREFIX System.OsPath
#endif

module System.Process.Posix.OsString.Compat
    ( OsString.toChar
    , System.Process.Posix.OsString.Compat.all
    , System.Process.Posix.OsString.Compat.cons
    , System.Process.Posix.OsString.Compat.elem
    , System.Process.Posix.OsString.Compat.foldr
    , System.Process.Posix.OsString.Compat.fromWord
    , System.Process.Posix.OsString.Compat.null
    ) where

import Data.Coerce (coerce)
import Data.Word (Word8)

import
  qualified BYTESTRING_MODULE_PREFIX.Data.ByteString.Short
  as ShortByteString
import OS_STRING_PACKAGE System.OsString.Internal.Types
  ( OsChar(OsChar)
  , OsString(OsString)
  , PosixChar(PosixChar)
  , PosixString(PosixString)
  )
import qualified OS_STRING_PACKAGE System.OsString as OsString

all :: (OsChar -> Bool) -> OsString -> Bool
all = coerce ShortByteString.all

cons :: OsChar -> OsString -> OsString
cons = coerce ShortByteString.cons

elem :: OsChar -> OsString -> Bool
elem = coerce ShortByteString.elem

foldr :: forall a . (OsChar -> a -> a) -> a -> OsString -> a
foldr = coerce (ShortByteString.foldr @a)

fromWord :: Word8 -> OsChar
fromWord = OsChar . PosixChar

null :: OsString -> Bool
null = coerce ShortByteString.null
