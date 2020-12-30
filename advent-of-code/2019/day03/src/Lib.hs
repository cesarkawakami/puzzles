module Lib (run, doPart1, doPart2, doBench) where

import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import System.IO (stdout, hFlush)

data Move = MRight !Int | MDown !Int | MLeft !Int | MUp !Int

data Coord = Coord !Int !Int deriving (Show, Eq, Ord)

data CoordDist = CoordDist !Coord !Int deriving (Show, Eq, Ord)

wireCoords :: [Move] -> [CoordDist]
wireCoords =
  go 0 0 0
  where
    go x y d [] = [CoordDist (Coord x y) d]
    go x y d (MRight v : tl) = go2 x y d 1 0 v tl
    go x y d (MDown v : tl) = go2 x y d 0 (-1) v tl
    go x y d (MLeft v : tl) = go2 x y d (-1) 0 v tl
    go x y d (MUp v : tl) = go2 x y d 0 1 v tl
    go2 x y d _ _ 0 tl = go x y d tl
    go2 x y d dx dy t tl = CoordDist (Coord x y) d : go2 (x + dx) (y + dy) (d + 1) dx dy (t - 1) tl

parseMove :: T.Text -> Move
parseMove s = case T.unpack s of
  ('R' : tl) -> MRight (read tl)
  ('D' : tl) -> MDown (read tl)
  ('L' : tl) -> MLeft (read tl)
  ('U' : tl) -> MUp (read tl)
  _ -> error (printf "not a move: %s" s)

parseWire :: T.Text -> [Move]
parseWire line = map parseMove $ T.splitOn (T.pack ",") $ T.strip line

parseInput :: T.Text -> ([Move], [Move])
parseInput input = case T.lines input of
  [line1, line2] -> (parseWire line1, parseWire line2)
  _ -> error (printf "can't parse %s" input)

manhattan :: CoordDist -> Int
manhattan (CoordDist (Coord x y) _) = abs x + abs y

commonMergeDistances :: [CoordDist] -> [CoordDist] -> [CoordDist]
commonMergeDistances [] _ = []
commonMergeDistances _ [] = []
commonMergeDistances as@((CoordDist acoord adist) : arest) bs@((CoordDist bcoord bdist) : brest) =
  case compare acoord bcoord of
    LT -> commonMergeDistances arest bs
    GT -> commonMergeDistances as brest
    EQ -> CoordDist acoord (adist + bdist) : commonMergeDistances arest brest

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let (moves1, moves2) = parseInput input
      coords1 = L.sort $ drop 1 $ wireCoords moves1
      coords2 = L.sort $ drop 1 $ wireCoords moves2
      inters = commonMergeDistances coords1 coords2
      (CoordDist (Coord closestX closestY) _) = L.minimumBy (comparing manhattan) inters
      closestManhattan = manhattan (CoordDist (Coord closestX closestY) 0)
  printf "part1, (x, y): (%d, %d), dist: %d\n" closestX closestY closestManhattan
  hFlush stdout

doPart2 :: T.Text -> IO ()
doPart2 input = do
  let (moves1, moves2) = parseInput input
      coords1 = L.sort $ drop 1 $ wireCoords moves1
      coords2 = L.sort $ drop 1 $ wireCoords moves2
      inters = commonMergeDistances coords1 coords2
      (CoordDist (Coord closestX closestY) closestDist) =
        L.minimumBy (comparing (\(CoordDist _ d) -> d)) inters
  printf "part2, (x, y): (%d, %d), dist: %d\n" closestX closestY closestDist
  hFlush stdout

run :: IO ()
run = do
  input <- TIO.getContents
  doPart1 input
  doPart2 input

doBench :: IO ()
doBench = TIO.readFile "input" >>= doPart1
