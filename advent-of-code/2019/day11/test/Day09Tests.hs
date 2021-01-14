{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day09Tests (tests) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import IntMachine (makeStateWithInput, readMemory, simulateOutput)
import Test.HUnit (Test (..), (~:), (~?=))

_programTextData :: T.Text
_programTextData = TE.decodeUtf8 $(makeRelativeToProject "test/day09input.txt" >>= embedFile)

simulateInputToOutput :: T.Text -> [Int64] -> [Int64]
simulateInputToOutput programData input =
  simulateOutput $ makeStateWithInput input $ readMemory programData

tests :: Test
tests =
  "day9"
    ~: TestList
      [ "part1 ex1"
          ~: simulateInputToOutput "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" []
            ~?= [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99],
        "part1 ex3"
          ~: simulateInputToOutput "104,1125899906842624,99" [] ~?= [1125899906842624],
        "part1 final" ~: simulateInputToOutput _programTextData [1] ~?= [2457252183],
        "part2 final" ~: simulateInputToOutput _programTextData [2] ~?= [70634]
      ]
