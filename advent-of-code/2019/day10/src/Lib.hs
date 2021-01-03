module Lib (run) where

import Data.List (groupBy, maximumBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)

type Point = (Int, Int)

halfPlaneOf :: Point -> Int
halfPlaneOf (x, y)
  | x > 0 = 0
  | x == 0 && y > 0 = 0
  | x == 0 && y < 0 = 1
  | x < 0 = 1
  | otherwise = undefined

zCross :: Point -> Point -> Int
zCross (ax, ay) (bx, by) = ax * by - ay * bx

comparePsClosest :: Point -> Point -> Ordering
comparePsClosest = comparePsGeneric (\(ax, ay) (bx, by) -> compare (sq ax + sq ay) (sq bx + sq by))
  where
    sq = (^ (2 :: Int))

comparePsAngle :: Point -> Point -> Ordering
comparePsAngle = comparePsGeneric (\_ _ -> EQ)

comparePsGeneric :: (Point -> Point -> Ordering) -> Point -> Point -> Ordering
comparePsGeneric disambF a@(ax, ay) b@(bx, by)
  | halfPlaneOf a /= halfPlaneOf b = halfPlaneOf a `compare` halfPlaneOf b
  | cross < 0 = LT
  | cross > 0 = GT
  | cross == 0 = disambF a b
  | otherwise = undefined
  where
    cross = zCross (ax, ay) (bx, by)

angleEq :: Point -> Point -> Bool
angleEq a b = comparePsAngle a b == EQ

visiClusters :: [Point] -> [[Point]]
visiClusters = groupBy angleEq . sortBy comparePsClosest

visiClustersFrom :: [Point] -> Point -> [[Point]]
visiClustersFrom ps (px, py) = visiClusters shifted
  where
    shifted = [(x - px, y - py) | (x, y) <- ps, (x, y) /= (px, py)]

countAround :: [Point] -> Point -> Int
countAround ps p = length $ visiClustersFrom ps p

inputToCoords :: T.Text -> [Point]
inputToCoords input =
  [ (x, y)
    | (y, line) <- zip [0, -1 ..] $ T.lines input,
      (x, ch) <- zip [0 ..] $ T.unpack line,
      ch == '#'
  ]

consumeGroups :: [[Point]] -> [Point]
consumeGroups lst = go lst []
  where
    go [] [] = []
    go [] revBack = go (reverse revBack) []
    go ([] : otherGroups) revBack = go otherGroups revBack
    go ((groupHead : groupTail) : otherGroups) revBack =
      groupHead : go otherGroups (groupTail : revBack)

solvePart1 :: T.Text -> IO ()
solvePart1 input = do
  let coords = inputToCoords input
      (bestX, bestY) = maximumBy (comparing $ countAround coords) coords
      bestCount = countAround coords (bestX, bestY)
  printf "part1, best p: (%d, %d), count: %d\n" bestX bestY bestCount

solvePart2 :: T.Text -> IO ()
solvePart2 input = do
  let coords = inputToCoords input
      (bestX, bestY) = maximumBy (comparing $ countAround coords) coords
      vaporized =
        map (\(x, y) -> (bestX + x, bestY + y)) $ consumeGroups $ visiClustersFrom coords (bestX, bestY)
      (ansX, ansY) = vaporized !! (200 - 1)
  printf "part2, sequence: %s\n" (show $ take 10 vaporized)
  printf "part2, 200th: (%d, %d)\n" ansX ansY
  printf "part2, answer: %d\n" (ansX * 100 - ansY)

run :: IO ()
run = do
  input <- TIO.getContents
  solvePart1 input
  solvePart2 input
