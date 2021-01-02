{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day07Tests (tests) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import IntMachine (makeStateWithInput, readMemory, simulateOutput)
import Test.HUnit (Test (..), (~:), (~?=))
import Text.Printf (printf)

programTextData :: T.Text
programTextData = TE.decodeUtf8 $(makeRelativeToProject "test/day07input.txt" >>= embedFile)

simulateInputToOutput :: Map Int Int -> [Int] -> [Int]
simulateInputToOutput memory input = simulateOutput $ makeStateWithInput input memory

expectSingle :: [Int] -> Int
expectSingle [a] = a
expectSingle a = error (printf "expected single element, got %s" (show a))

simulateThrusterSettings :: Map Int Int -> (Int, Int, Int, Int, Int) -> Int
simulateThrusterSettings memory (t1, t2, t3, t4, t5) =
  p5out
  where
    p1out = expectSingle $ simulateInputToOutput memory [t1, 0]
    p2out = expectSingle $ simulateInputToOutput memory [t2, p1out]
    p3out = expectSingle $ simulateInputToOutput memory [t3, p2out]
    p4out = expectSingle $ simulateInputToOutput memory [t4, p3out]
    p5out = expectSingle $ simulateInputToOutput memory [t5, p4out]

simulateThrusterSettingsFeedback :: Map Int Int -> (Int, Int, Int, Int, Int) -> Int
simulateThrusterSettingsFeedback memory (t1, t2, t3, t4, t5) =
  last p5out
  where
    p1out = simulateInputToOutput memory $ t1 : 0 : p5out
    p2out = simulateInputToOutput memory $ t2 : p1out
    p3out = simulateInputToOutput memory $ t3 : p2out
    p4out = simulateInputToOutput memory $ t4 : p3out
    p5out = simulateInputToOutput memory $ t5 : p4out

allCombinations :: Int -> Int -> [(Int, Int, Int, Int, Int)]
allCombinations lower upper =
  [ (t1, t2, t3, t4, t5)
    | t1 <- [lower .. upper],
      t2 <- [lower .. upper],
      t1 /= t2,
      t3 <- [lower .. upper],
      t3 /= t1,
      t3 /= t2,
      t4 <- [lower .. upper],
      t4 /= t3,
      t4 /= t2,
      t4 /= t1,
      t5 <- [lower .. upper],
      t5 /= t4,
      t5 /= t3,
      t5 /= t2,
      t5 /= t1
  ]

solvePart1 :: T.Text -> ((Int, Int, Int, Int, Int), Int)
solvePart1 input =
  (bestComb, bestAnswer)
  where
    memory = readMemory input
    bestComb = maximumBy (comparing $ simulateThrusterSettings memory) $ allCombinations 0 4
    bestAnswer = simulateThrusterSettings memory bestComb

solvePart2 :: T.Text -> ((Int, Int, Int, Int, Int), Int)
solvePart2 input =
  (bestComb, bestAnswer)
  where
    memory = readMemory input
    bestComb = maximumBy (comparing $ simulateThrusterSettingsFeedback memory) $ allCombinations 5 9
    bestAnswer = simulateThrusterSettingsFeedback memory bestComb

tests :: Test
tests =
  "day07"
    ~: TestList
      [ "part1 ex1"
          ~: solvePart1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
            ~?= ((4, 3, 2, 1, 0), 43210),
        "part1 ex2"
          ~: solvePart1 "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
            ~?= ((0, 1, 2, 3, 4), 54321),
        "part1 ex3"
          ~: solvePart1
            "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\
            \1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
            ~?= ((1, 0, 4, 3, 2), 65210),
        "part1 final" ~: solvePart1 programTextData ~?= ((2, 4, 3, 0, 1), 17440),
        "part2 ex1"
          ~: solvePart2
            "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\
            \27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
            ~?= ((9, 8, 7, 6, 5), 139629729),
        "part2 ex2"
          ~: solvePart2
            "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\
            \-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\
            \53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
            ~?= ((9, 7, 8, 5, 6), 18216),
        "part2 final" ~: solvePart2 programTextData ~?= ((7, 9, 5, 8, 6), 27561242)
      ]
