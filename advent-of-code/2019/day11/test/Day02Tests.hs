{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day02Tests (tests) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import IntMachine (FinalState (..), makeState, patchState, readMemory, simulateFinal)
import Test.HUnit (Test (..), (~:), (~?=))

inputBytes :: T.Text
inputBytes = TE.decodeUtf8 $(makeRelativeToProject "test/day02input.txt" >>= embedFile)

simulateToList :: T.Text -> [Int64]
simulateToList input = memList
  where
    FinalState memList _ = simulateFinal $ makeState $ readMemory input

day02Part1Answer :: Int64
day02Part1Answer = head memList
  where
    FinalState memList _ =
      simulateFinal $ patchState [(1, 12), (2, 2)] $ makeState $ readMemory inputBytes

day02Part2Answer :: Int64
day02Part2Answer = head memList
  where
    FinalState memList _ =
      simulateFinal $ patchState [(1, 76), (2, 3)] $ makeState $ readMemory inputBytes

tests :: Test
tests =
  TestLabel "day02" $
    TestList
      [ "main example"
          ~: simulateToList "1,9,10,3,2,3,11,0,99,30,40,50"
          ~?= [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50],
        "example 1" ~: simulateToList "1,0,0,0,99" ~?= [2, 0, 0, 0, 99],
        "example 2" ~: simulateToList "2,3,0,3,99" ~?= [2, 3, 0, 6, 99],
        "example 3" ~: simulateToList "2,4,4,5,99,0" ~?= [2, 4, 4, 5, 99, 9801],
        "example 4" ~: simulateToList "1,1,1,4,99,5,6,0,99" ~?= [30, 1, 1, 4, 2, 5, 6, 0, 99],
        "part1" ~: day02Part1Answer ~?= 5534943,
        "part2" ~: day02Part2Answer ~?= 19690720
      ]
