import qualified Day02Tests
import qualified Day05Tests
import qualified Day07Tests
import qualified Day09Tests
import Test.HUnit (Test (..), runTestTT)

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ Day02Tests.tests,
          Day05Tests.tests,
          Day07Tests.tests,
          Day09Tests.tests
        ]
  return ()
