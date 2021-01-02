{-# LANGUAGE OverloadedStrings #-}

module Lib (run) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Debug.Trace (trace)

readInput :: T.Text -> Map T.Text [T.Text]
readInput input = Map.fromListWith (++) $ map readLine $ T.lines input
  where
    readLine line = case T.splitOn (T.pack ")") line of
      [left, right] -> (left, [right])
      _ -> undefined

orbitCounts :: Map T.Text [T.Text] -> T.Text -> (Int, Int)
orbitCounts adj cur = (subDirect + length children, subIndirect + subDirect + length children)
  where
    children = Map.findWithDefault [] cur adj
    (subDirect, subIndirect) =
      foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) $
        map (orbitCounts adj) children

data DistRes
  = Neither
  | JustLeft !Int
  | JustRight !Int
  | Both !Int
  deriving Show

distance :: Map T.Text [T.Text] -> T.Text -> T.Text -> T.Text -> DistRes
distance adj left right cur
  | cur == left = trace (printf "%s: %s" (show cur) (show $ JustLeft 0)) JustLeft 0
  | cur == right = trace (printf "%s: %s" (show cur) (show $ JustRight 0))JustRight 0
  | otherwise = trace (printf "%s: %s" (show cur) (show $ update subresult)) update subresult
  where
    children = Map.findWithDefault [] cur adj
    subresult = foldl' combine Neither $ map (distance adj left right) children
    ohno a b = error (printf "combining %s and %s" (show a) (show b))
    combine Neither x = x
    combine x Neither = x
    combine a@(Both _) b = ohno a b
    combine a b@(Both _) = ohno a b
    combine a@(JustLeft _) b@(JustLeft _) = ohno a b
    combine a@(JustRight _) b@(JustRight _) = ohno a b
    combine (JustLeft d) (JustRight d') = Both (d + d')
    combine (JustRight d) (JustLeft d') = Both (d + d')
    update Neither = Neither
    update (JustLeft d) = JustLeft (d + 1)
    update (JustRight d) = JustRight (d + 1)
    update (Both d) = Both d

solvePart1 :: T.Text -> IO ()
solvePart1 input = do
  let adj = readInput input
      (direct, indirect) = orbitCounts adj (T.pack "COM")
  printf "part1, direct: %d, indirect: %d\n" direct indirect

solvePart2 :: T.Text -> IO ()
solvePart2 input = do
  let adj = readInput input
      answer = case distance adj "YOU" "SAN" "COM" of
        Both d -> d
        other -> error (printf "oh no got %s" (show other))
  printf "part2, answer: %d\n" answer

run :: IO ()
run = do
  input <- TIO.getContents
  solvePart1 input
  solvePart2 input
