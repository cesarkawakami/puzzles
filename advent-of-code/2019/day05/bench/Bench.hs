import Criterion (bench, nfIO)
import Criterion.Main (defaultMain)

import qualified Data.Text.IO as TIO

doRun :: IO ()
doRun = do
    input <- TIO.readFile "input"
    error "need to do something"

main :: IO ()
main = do
    defaultMain [ bench "run" $ nfIO doRun ]
