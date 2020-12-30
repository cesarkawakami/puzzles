{-# LANGUAGE BangPatterns #-}

module LibVector (run, doPart1, doBench) where

import Control.Monad (forM_)
import Data.Int (Int32)
import qualified Data.List as L
import Data.Ord (comparing)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Text.Printf (printf)

data Move = MRight !Int | MDown !Int | MLeft !Int | MUp !Int

wireCoords :: [Move] -> VU.Vector (Int32, Int32)
wireCoords moves =
  VU.create $ do
    let initialCapacity = 160000
    output <- VUM.new initialCapacity
    refOutputLen <- newSTRef (0 :: Int)
    refCurX <- newSTRef (0 :: Int32)
    refCurY <- newSTRef (0 :: Int32)

    forM_ moves $ \move -> do
      let (dx, dy, t) = case move of
            MRight v -> (1, 0, v)
            MLeft v -> (-1, 0, v)
            MUp v -> (0, 1, v)
            MDown v -> (0, -1, v)

      forM_ [1 .. t] $ \_ -> do
        curX <- readSTRef refCurX
        curY <- readSTRef refCurY
        let newX = curX + dx
            newY = curY + dy
        outputLen <- readSTRef refOutputLen
        VUM.write output outputLen (newX, newY)
        writeSTRef refOutputLen (outputLen + 1)
        writeSTRef refCurX newX
        writeSTRef refCurY newY

    finalOutputLen <- readSTRef refOutputLen
    let finalOutput = VUM.slice 0 finalOutputLen output
    VAI.sort finalOutput
    return finalOutput

wireInters :: [Move] -> [Move] -> [(Int32, Int32)]
wireInters moves1 moves2 =
  let coords1 = wireCoords moves1
      coords2 = wireCoords moves2
      go !i !j
        | (i == VU.length coords1) || (j == VU.length coords2) = []
        | otherwise = case (coords1 VU.! i, coords2 VU.! j) of
          (vi, vj) | vi < vj -> go (i + 1) j
          (vi, vj) | vi > vj -> go i (j + 1)
          (vi, _) | otherwise -> vi : go (i + 1) (j + 1)
   in go 0 0

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

manhattan :: (Int32, Int32) -> Int
manhattan (!x, !y) = fromIntegral $ abs x + abs y

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let (moves1, moves2) = parseInput input
      inters = wireInters moves1 moves2
      (closestX, closestY) = L.minimumBy (comparing manhattan) inters
      closestManhattan = manhattan (closestX, closestY)
  printf "dist: %d, " closestManhattan

run :: IO ()
run = do
  input <- TIO.getContents
  doPart1 input

doBench :: IO ()
doBench = TIO.readFile "input" >>= doPart1
