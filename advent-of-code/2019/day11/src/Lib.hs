module Lib (run) where

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import IntMachine (makeStateWithInput, readMemory, simulateOutput)
import Text.Printf (printf)

data Pt = Pt Int64 Int64 deriving (Show)

instance Eq Pt where
  (Pt ax ay) == (Pt bx by) = (ax, ay) == (bx, by)

instance Ord Pt where
  compare (Pt ax ay) (Pt bx by) = compare (ax, ay) (bx, by)

data State = State Pt Pt (Set Pt) (Set Pt) deriving (Show)

data Color = White | Black deriving (Show)

ptAdd :: Pt -> Pt -> Pt
ptAdd (Pt ax ay) (Pt bx by) = Pt (ax + bx) (ay + by)

ptRotLeft :: Pt -> Pt
ptRotLeft (Pt x y) = Pt (- y) x

ptRotRight :: Pt -> Pt
ptRotRight = ptRotLeft . ptRotLeft . ptRotLeft -- lol?

simulateInputToOutput :: T.Text -> [Int64] -> [Int64]
simulateInputToOutput programData input =
  simulateOutput $ makeStateWithInput input $ readMemory programData

rotIdToRot :: Int64 -> Pt -> Pt
rotIdToRot 0 = ptRotLeft
rotIdToRot 1 = ptRotRight
rotIdToRot x = error (printf "unexpected rot id %d" x)

colorIdToColor :: Int64 -> Color
colorIdToColor 0 = Black
colorIdToColor 1 = White
colorIdToColor x = error (printf "unexpected color id %d" x)

simulateRobot :: State -> [Int64] -> ([Int64], [State])
simulateRobot lastState [] = ([], [lastState])
simulateRobot _ [x] = error (printf "got only one input (%d), but expected 2" x)
simulateRobot state@(State p dp whites painteds) (colorId : rotId : progOutTl) =
  trace
    ( printf
        "p:%s colorId:%d rotId:%d newP:%s newDp:%s iTR:%d #newWhites:%d #newPainteds:%d"
        (show p)
        colorId
        rotId
        (show newP)
        (show newDp)
        inputToRobot
        (Set.size newWhites)
        (Set.size newPainteds)
    )
    (inputToRobot : subProgIn, state : subStates)
  where
    newDp = rotIdToRot rotId dp
    newP = p `ptAdd` newDp
    newWhites = case colorIdToColor colorId of
      White -> Set.insert p whites
      Black -> whites
    newPainteds = Set.insert p painteds
    inputToRobot = if Set.member newP newWhites then 1 else 0
    (subProgIn, subStates) = simulateRobot (State newP newDp newWhites newPainteds) progOutTl

solvePart1 :: T.Text -> IO ()
solvePart1 input = do
  let (progInput, allStates) = simulateRobot (State (Pt 0 0) (Pt 0 1) Set.empty Set.empty) progOutput
      progOutput = simulateInputToOutput input (0 : progInput)
      (State _ _ _ finalPainteds) = last allStates
  forM_ allStates $ \(State (Pt px py) (Pt dx dy) whites painteds) -> do
    return ()
  -- printf
  --   "state -> p:(%d, %d) dp:(%d, %d) whites:%s painteds: %s\n"
  --   px
  --   py
  --   dx
  --   dy
  --   (show whites)
  --   (show painteds)
  printf "part1, answer: %d\n" (Set.size finalPainteds)

run :: IO ()
run = do
  input <- TIO.getContents
  solvePart1 input
