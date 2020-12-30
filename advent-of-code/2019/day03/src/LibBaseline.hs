module LibBaseline (run, doPart1, doBench) where

import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)

data Move = MRight Int | MDown Int | MLeft Int | MUp Int

wireCoords :: [Move] -> [(Int, Int)]
wireCoords =
  go 0 0
  where
    go x y [] = [(x, y)]
    go x y (MRight 0 : tl) = go x y tl
    go x y (MDown 0 : tl) = go x y tl
    go x y (MLeft 0 : tl) = go x y tl
    go x y (MUp 0 : tl) = go x y tl
    go x y (MRight v : tl) = (x, y) : go (x + 1) y (MRight (v - 1) : tl)
    go x y (MDown v : tl) = (x, y) : go x (y - 1) (MDown (v - 1) : tl)
    go x y (MLeft v : tl) = (x, y) : go (x - 1) y (MLeft (v - 1) : tl)
    go x y (MUp v : tl) = (x, y) : go x (y + 1) (MUp (v - 1) : tl)

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

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

sortedInters :: (Ord a) => [a] -> [a] -> [a]
sortedInters [] _ = []
sortedInters _ [] = []
sortedInters (x : xs) (y : ys) =
  case compare x y of
    LT -> sortedInters xs (y : ys)
    GT -> sortedInters (x : xs) ys
    EQ -> x : sortedInters xs ys

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let (moves1, moves2) = parseInput input
      coords1 = L.sort $ drop 1 $ wireCoords moves1
      coords2 = L.sort $ drop 1 $ wireCoords moves2
      inters = sortedInters coords1 coords2
      (closestX, closestY) = L.minimumBy (comparing manhattan) $ inters
      closestManhattan = manhattan (closestX, closestY)
  printf "dist: %d, " closestManhattan

run :: IO ()
run = do
  input <- TIO.getContents
  doPart1 input

doBench :: IO ()
doBench = TIO.readFile "input" >>= doPart1
