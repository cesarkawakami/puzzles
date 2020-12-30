module LibSet (run, doPart1, doBench) where

import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)

data Move = MRight !Int | MDown !Int | MLeft !Int | MUp !Int

wireCoords :: [Move] -> S.Set (Int, Int)
wireCoords moves =
  let goWalk acc x y (MRight 0) = (x, y, acc)
      goWalk acc x y (MDown 0) = (x, y, acc)
      goWalk acc x y (MLeft 0) = (x, y, acc)
      goWalk acc x y (MUp 0) = (x, y, acc)
      goWalk acc x y (MRight v) = goWalk ((x + 1, y) : acc) (x + 1) y (MRight (v - 1))
      goWalk acc x y (MDown v) = goWalk ((x, y - 1) : acc) x (y - 1) (MDown (v - 1))
      goWalk acc x y (MLeft v) = goWalk ((x - 1, y) : acc) (x - 1) y (MLeft (v - 1))
      goWalk acc x y (MUp v) = goWalk ((x, y + 1) : acc) x (y + 1) (MUp (v - 1))
      go acc _ _ [] = acc
      go acc x y (move : tl) =
        let (newX, newY, newAcc) = goWalk acc x y move
         in go newAcc newX newY tl
   in S.fromList $ go [] 0 0 moves

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

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let (moves1, moves2) = parseInput input
      coords1 = wireCoords moves1
      coords2 = wireCoords moves2
      inters = S.intersection coords1 coords2
      (closestX, closestY) = L.minimumBy (comparing manhattan) $ S.toList inters
      closestManhattan = manhattan (closestX, closestY)
  printf "dist: %d, " closestManhattan

run :: IO ()
run = do
  input <- TIO.getContents
  doPart1 input

doBench :: IO ()
doBench = TIO.readFile "input" >>= doPart1
