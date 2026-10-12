import qualified Test.OsString
import qualified Test.String

main :: IO ()
main = do
    Test.String.tests
    Test.OsString.tests
