import qualified Day02Tests
import qualified Day05Tests
import Test.HUnit (Test (..), runTestTT)

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList [Day02Tests.tests, Day05Tests.tests]
  return ()
