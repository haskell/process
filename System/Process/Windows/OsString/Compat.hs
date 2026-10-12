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

module System.Process.Windows.OsString.Compat
    ( OsString.toChar
    , OsString.unpack
    , System.Process.Windows.OsString.Compat.cons
    , System.Process.Windows.OsString.Compat.dropWhileEnd
    , System.Process.Windows.OsString.Compat.elem
    , System.Process.Windows.OsString.Compat.foldr
    , System.Process.Windows.OsString.Compat.fromWord
    , System.Process.Windows.OsString.Compat.intercalate
    ) where

import Data.Coerce (coerce)
import Data.Word (Word8)

import
  qualified BYTESTRING_MODULE_PREFIX.Data.ByteString.Short.Word16
  as ShortByteString
import OS_STRING_PACKAGE System.OsString.Internal.Types
  ( OsChar(OsChar)
  , OsString(OsString)
  , WindowsChar(WindowsChar)
  , WindowsString(WindowsString)
  )
import qualified OS_STRING_PACKAGE System.OsString as OsString

cons :: OsChar -> OsString -> OsString
cons = coerce ShortByteString.cons

dropWhileEnd :: (OsChar -> Bool) -> OsString -> OsString
dropWhileEnd = coerce ShortByteString.dropWhileEnd

elem :: OsChar -> OsString -> Bool
elem = coerce ShortByteString.elem

foldr :: forall a . (OsChar -> a -> a) -> a -> OsString -> a
foldr = coerce (ShortByteString.foldr @a)

fromWord :: Word8 -> OsChar
fromWord = OsChar . WindowsChar . fromIntegral

intercalate :: OsString -> [OsString] -> OsString
intercalate = coerce ShortByteString.intercalate
