module Lib (run) where

import Data.List (foldl')
import Debug.Trace (trace)
import Text.Printf (printf)

fuelReq :: Int -> Int
fuelReq mass =
  let result = max 0 $ mass `div` 3 - 2
   in trace (printf "mass of %d: %d" mass result) result

rocketFuelReq :: Int -> Int
rocketFuelReq mass | mass <= 0 = 0
rocketFuelReq mass =
  let selfFuelReq = fuelReq mass
      answer = selfFuelReq + rocketFuelReq selfFuelReq
   in trace (printf "rocket mass for %d: %d" mass answer) answer

run :: IO ()
run = do
  contents <- getContents
  let answer = foldl' (+) 0 $ map (fuelReq . read) $ lines contents
   in printf "part1, answer: %d\n" answer
  let answer = foldl' (+) 0 $ map (rocketFuelReq . read) $ lines contents
   in printf "part2, answer: %d\n" answer
