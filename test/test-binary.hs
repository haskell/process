-- Test suite for System.Process.Binary.

import System.Process.Binary (ByteString)
import qualified System.Process.Binary as B

import Test.HUnit
import System.Exit
import Control.Monad


-- An arbitrary sequence of bytes (randomly generated).
someBytes :: ByteString
someBytes = B.pack [
  0x01, 0xc0, 0xc3, 0xbf, 0xe3, 0xa9, 0xc6, 0xb1,
  0xa7, 0x56, 0x4f, 0x9e, 0xd6, 0x06, 0xb8, 0x27,
  0xed, 0xdf, 0x93, 0x1a, 0x0b, 0x1a, 0x95, 0x24,
  0x3a, 0xf1, 0xb0, 0x2f, 0x39, 0x8d, 0x7b, 0x2d]

-- An invalid UTF8 code.
invalidUtf8 :: ByteString
invalidUtf8 = B.pack [0xd8, 0x00]

testCat :: String -> ByteString -> Assertion
testCat name x = do
  x2 <- B.readProcess "cat" [] x
  assertEqual name x x2

testCatWithExitCode :: String -> ByteString -> Assertion
testCatWithExitCode name x = do
  result <- B.readProcessWithExitCode "cat" [] x
  assertEqual name result (ExitSuccess, x, B.empty)


testSuite :: Test
testSuite = TestList . map TestCase $
  [testCat "cat1" someBytes
  ,testCat "cat2" invalidUtf8
  ,testCatWithExitCode "catWithExitCode1" someBytes
  ,testCatWithExitCode "catWithExitCode2" invalidUtf8
  ]

main :: IO ()
main = do
  r <- runTestTT testSuite
  when (errors r > 0 || failures r > 0) $
    exitFailure
