{-# LANGUAGE BangPatterns #-}

module LibHashSet (run, doPart1, doBench) where

import qualified Data.HashSet as HS
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)

data Move = MRight !Int | MDown !Int | MLeft !Int | MUp !Int

data Coord = Coord !Int !Int deriving (Eq)

instance Hashable Coord where
  hashWithSalt salt (Coord x y) = salt `hashWithSalt` x `hashWithSalt` y

wireCoords :: [Move] -> HS.HashSet Coord
wireCoords =
  go 0 0
  where
    go x y [] = HS.singleton (Coord x y)
    go x y (MRight 0 : tl) = go x y tl
    go x y (MDown 0 : tl) = go x y tl
    go x y (MLeft 0 : tl) = go x y tl
    go x y (MUp 0 : tl) = go x y tl
    go x y (MRight !v : tl) = HS.insert (Coord x y) $ go (x + 1) y (MRight (v - 1) : tl)
    go x y (MDown !v : tl) = HS.insert (Coord x y) $ go x (y - 1) (MDown (v - 1) : tl)
    go x y (MLeft !v : tl) = HS.insert (Coord x y) $ go (x - 1) y (MLeft (v - 1) : tl)
    go x y (MUp !v : tl) = HS.insert (Coord x y) $ go x (y + 1) (MUp (v - 1) : tl)

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

coordToPair :: Coord -> (Int, Int)
coordToPair (Coord x y) = (x, y)

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let (moves1, moves2) = parseInput input
      coords1 = wireCoords moves1
      coords2 = wireCoords moves2
      inters = L.map coordToPair $ HS.toList $ HS.intersection coords1 coords2
      (closestX, closestY) = L.minimumBy (comparing manhattan) inters
      closestManhattan = manhattan (closestX, closestY)
  printf "dist: %d, " closestManhattan

run :: IO ()
run = do
  input <- TIO.getContents
  doPart1 input

doBench :: IO ()
doBench = TIO.readFile "input" >>= doPart1
