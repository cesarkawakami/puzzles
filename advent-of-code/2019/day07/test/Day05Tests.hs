{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day05Tests (tests) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import IntMachine (FinalState (..), makeState, makeStateWithInput, readMemory, simulateFinal)
import Test.HUnit (Test (..), (~:), (~?=))

programTextData :: T.Text
programTextData = TE.decodeUtf8 $(makeRelativeToProject "test/day05input.txt" >>= embedFile)

simulateToList :: T.Text -> [Int]
simulateToList programText = memList
  where
    FinalState memList _ = simulateFinal $ makeState $ readMemory programText

simulateInputToOutput :: T.Text -> [Int] -> [Int]
simulateInputToOutput programText input = output
  where
    FinalState _ output = simulateFinal $ makeStateWithInput input $ readMemory programText

part2LargerExampleData :: T.Text
part2LargerExampleData =
  "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
  \1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
  \999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

tests :: Test
tests =
  TestLabel "day05" $
    TestList
      [ "part1 example" ~: simulateToList "1002,4,3,4,33" ~?= [1002, 4, 3, 4, 99],
        "part1 final"
          ~: simulateInputToOutput programTextData [1]
            ~?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 5346030],
        "part2 ex position mode eq"
          ~: TestList
            [ simulateInputToOutput "3,9,8,9,10,9,4,9,99,-1,8" [7] ~?= [0],
              simulateInputToOutput "3,9,8,9,10,9,4,9,99,-1,8" [8] ~?= [1],
              simulateInputToOutput "3,9,8,9,10,9,4,9,99,-1,8" [9] ~?= [0]
            ],
        "part2 ex position mode lt"
          ~: TestList
            [ simulateInputToOutput "3,9,7,9,10,9,4,9,99,-1,8" [7] ~?= [1],
              simulateInputToOutput "3,9,7,9,10,9,4,9,99,-1,8" [8] ~?= [0],
              simulateInputToOutput "3,9,7,9,10,9,4,9,99,-1,8" [9] ~?= [0]
            ],
        "part2 ex immediate mode eq"
          ~: TestList
            [ simulateInputToOutput "3,3,1108,-1,8,3,4,3,99" [7] ~?= [0],
              simulateInputToOutput "3,3,1108,-1,8,3,4,3,99" [8] ~?= [1],
              simulateInputToOutput "3,3,1108,-1,8,3,4,3,99" [9] ~?= [0]
            ],
        "part2 ex immediate mode lt"
          ~: TestList
            [ simulateInputToOutput "3,3,1107,-1,8,3,4,3,99" [7] ~?= [1],
              simulateInputToOutput "3,3,1107,-1,8,3,4,3,99" [8] ~?= [0],
              simulateInputToOutput "3,3,1107,-1,8,3,4,3,99" [9] ~?= [0]
            ],
        "part2 ex position mode jump"
          ~: TestList
            [ simulateInputToOutput "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [-1] ~?= [1],
              simulateInputToOutput "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [0] ~?= [0],
              simulateInputToOutput "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [1] ~?= [1]
            ],
        "part2 ex immediate mode jump"
          ~: TestList
            [ simulateInputToOutput "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [-1] ~?= [1],
              simulateInputToOutput "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [0] ~?= [0],
              simulateInputToOutput "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [1] ~?= [1]
            ],
        "part2 larger example"
          ~: TestList
            [ simulateInputToOutput part2LargerExampleData [7] ~?= [999],
              simulateInputToOutput part2LargerExampleData [8] ~?= [1000],
              simulateInputToOutput part2LargerExampleData [9] ~?= [1001]
            ],
        "part2 final" ~: simulateInputToOutput programTextData [5] ~?= [513116]
      ]
