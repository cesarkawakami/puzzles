import Criterion (bench, perBatchEnvWithCleanup)
import Criterion.Main (defaultMain)
import qualified LibBaseline
import qualified LibBaselineOpt
import qualified LibHashSet
import qualified LibSet
import qualified LibVector
import Text.Printf (printf)

main :: IO ()
main =
  do
    let createEnv _ = return ()
    let cleanupEnv _ _ = printf "\n"
    let benchNewline f = perBatchEnvWithCleanup createEnv cleanupEnv (const f)
    defaultMain
      [ bench "Data.List" $ benchNewline LibBaseline.doBench,
        bench "Data.Vector" $ benchNewline LibVector.doBench,
        bench "Data.List opt" $ benchNewline LibBaselineOpt.doBench,
        bench "Data.Set" $ benchNewline LibSet.doBench,
        bench "Data.HashSet" $ benchNewline LibHashSet.doBench
      ]
