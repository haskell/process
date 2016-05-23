-- Test suite for binary I/O.

import System.Process

import Test.HUnit
import Control.Monad
import System.IO
import System.Exit
import Control.Exception (evaluate)
import Control.DeepSeq (rnf)


data StreamMode = TextMode | BinaryMode

pipeSpec :: StreamMode -> StdStream
pipeSpec TextMode = CreatePipe
pipeSpec BinaryMode = CreateBinaryPipe

testCreateProcessCat :: String -- name of this test case
                     -> StreamMode -- stdin
                     -> StreamMode -- stdout
                     -> String -- stdin
                     -> String -- expected stdout
                     -> Assertion
testCreateProcessCat name modeIn modeOut xIn xOut = do
  (Just hIn, Just hOut, Nothing, ph) <- createProcess
    (proc "cat" []){ std_in = pipeSpec modeIn,
                     std_out = pipeSpec modeOut }
  hPutStr hIn xIn
  hClose hIn
  xOut2 <- hGetContents hOut
  evaluate (rnf xOut2)
  hClose hOut
  exitCode <- waitForProcess ph
  assertEqual name (exitCode, xOut2) (ExitSuccess, xOut)

someString = "a\xf0\x1003\&b"
someStringUtf8 = "a\195\176\225\128\131b"
someStringTrunc = "a\xf0\x03\&b"

testSuite :: Test
testSuite = TestList . map TestCase $
  [testCreateProcessCat "testCreateProcessCat1"
     TextMode TextMode someString someString
   -- In binary mode, characters are truncated to 8-bits
   -- (c.f. documentation for 'hSetBinaryMode').
  ,testCreateProcessCat "testCreateProcessCat2"
     BinaryMode BinaryMode someString someStringTrunc
  ]
  ++
  -- The following tests are only valid in the utf-8 locale.
  (if show localeEncoding == "UTF-8"
   then [testCreateProcessCat "testCreateProcessCat3"
           TextMode BinaryMode someString someStringUtf8
        ,testCreateProcessCat "testCreateProcessCat4"
           BinaryMode TextMode someStringUtf8 someString
        ]
   else [])

main :: IO ()
main = do
  r <- runTestTT testSuite
  when (errors r > 0 || failures r > 0) $
    exitFailure
