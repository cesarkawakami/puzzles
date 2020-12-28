module Lib (run) where

import Control.Monad (foldM_)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Debug.Trace (trace)
import Text.Printf (printf)

data StateHalted = Continue | Halted deriving (Show)

data State = State (Map.Map Int Int) StateHalted Int deriving (Show)

executeArithOp :: State -> (Int -> Int -> Int) -> Int -> Int -> Int -> State
executeArithOp (State memory Continue pc) f lefti righti dest =
  let left = memory Map.! lefti
      right = memory Map.! righti
      result = f left right
   in State (Map.insert dest result memory) Continue (pc + 4)
executeArithOp state@(State _ Halted _) _ _ _ _ = state

executeOp :: State -> Int -> Int -> Int -> Int -> State
executeOp state@(State _ Halted _) _ _ _ _ = state
executeOp state 1 lefti righti dest = executeArithOp state (+) lefti righti dest
executeOp state 2 lefti righti dest = executeArithOp state (*) lefti righti dest
executeOp (State memory Continue pc) 99 _ _ _ = State memory Halted pc
executeOp (State _ Continue _) op _ _ _ = error (printf "invalid op: %d" op)

step :: State -> State
step state@(State _ Halted _) = state
step state@(State memory Continue pc) =
  let op = memory Map.! (pc + 0)
      arg1 = memory Map.! (pc + 1)
      arg2 = memory Map.! (pc + 2)
      arg3 = memory Map.! (pc + 3)
      nextState = executeOp state op arg1 arg2 arg3
  --  in trace (show nextState) nextState
   in nextState

runToHalt :: State -> State
runToHalt state =
  last $ takeWhilePlus1 shouldContinue $ L.iterate' step state
  where
    shouldContinue (State _ Continue _) = True
    shouldContinue (State _ Halted _) = False
    takeWhilePlus1 f lst =
      let (pre, post) = span f lst
       in pre ++ take 1 post

readState :: T.Text -> State
readState input =
  let numberList = map (read . T.unpack) $ T.splitOn (T.pack ",") $ T.strip input
      memory = L.foldl' f Map.empty $ zip [0 ..] numberList
        where
          f acc (i, v) = Map.insert i v acc
      state = State memory Continue 0
   in state

fudge :: Int -> Int -> State -> State
fudge in1 in2 (State memory halted pc) =
  let updatedMemory = Map.union (Map.fromList [(1, in1), (2, in2)]) memory
   in State updatedMemory halted pc

stateOutput :: State -> Int
stateOutput (State memory _ _) = memory Map.! 0

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let initialState = readState input
      finalState = runToHalt initialState
      finalOutput = stateOutput finalState
  printf "part1, final state: %s\n" (show finalState)
  printf "part1, output: %d\n" finalOutput

solveForOutput :: Int -> State -> (Int, Int)
solveForOutput expectedOutput state =
  let check (in1, in2) =
        let updatedInitialState = fudge in1 in2 state
            finalState = runToHalt updatedInitialState
            finalOutput = stateOutput finalState
         in finalOutput == expectedOutput
      candidates = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
      searchResult = L.find check candidates
   in case searchResult of
        Just (x, y) -> (x, y)
        Nothing -> error (printf "can't solve for %d" expectedOutput)

doPart2 :: T.Text -> IO ()
doPart2 input = do
  let originalState = readState input
      (x, y) = solveForOutput 19690720 originalState
  printf "part2, x: %d, y: %d\n" x y
  printf "part2, answer: %d\n" (100 * x + y)

run :: IO ()
run = do
  fullInput <- TIO.getContents
  let inputs = T.lines fullInput
  foldM_ (\_ input -> doPart1 input) () inputs
  foldM_ (\_ input -> doPart2 input) () inputs
