{-# LANGUAGE OverloadedStrings #-}

module Lib (run) where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Debug.Trace (trace)
import Text.Printf (printf)

trace :: p1 -> p2 -> p2
trace _ b = b

data RangeStatus = CheckRange | Free

data SeenStatus = Seen | Unseen

data CheckType = Part1 | Part2

countPass :: CheckType -> [Int] -> [Int] -> Int
countPass checkType lower upper =
  go [] Unseen lower upper CheckRange CheckRange
  where
    go digits Seen [] [] _ _ = trace (concatMap show $ reverse digits) 1
    go _ Unseen [] [] _ _ = 0
    go _ _ [] _ _ _ = undefined
    go _ _ _ [] _ _ = undefined
    go digits doubledStatus (lowerHd : lowerTl) (upperHd : upperTl) lowerStatus upperStatus =
      sum $ mapMaybe try_ [0 .. 9]
      where
        try_ nextDigit =
          case ( nextLowerStatus lowerHd lowerStatus nextDigit,
                 nextUpperStatus upperHd upperStatus nextDigit,
                 nextDoubledStatus doubledStatus checkType (nextDigit : digits) lowerTl,
                 increasingOk (nextDigit : digits)
               ) of
            (Just lowerStatus', Just upperStatus', doubledStatus', Just ()) ->
              Just $
                go (nextDigit : digits) doubledStatus' lowerTl upperTl lowerStatus' upperStatus'
            _ -> Nothing

    nextLowerStatus _ Free _ = Just Free
    nextLowerStatus lowerHd CheckRange nextDigit
      | nextDigit > lowerHd = Just Free
      | nextDigit == lowerHd = Just CheckRange
      | otherwise = Nothing

    nextUpperStatus _ Free _ = Just Free
    nextUpperStatus upperHd CheckRange nextDigit
      | nextDigit < upperHd = Just Free
      | nextDigit == upperHd = Just CheckRange
      | otherwise = Nothing

    nextDoubledStatus Seen _ _ _ = Seen
    nextDoubledStatus Unseen _ [] _ = undefined
    nextDoubledStatus Unseen Part1 [_] _ = Unseen
    nextDoubledStatus Unseen Part1 (d1 : d0 : _) _
      | d1 == d0 = Seen
      | otherwise = Unseen
    nextDoubledStatus Unseen Part2 [_] _ = Unseen
    nextDoubledStatus Unseen Part2 [_, _] _ = Unseen
    nextDoubledStatus Unseen Part2 [d2, d1, d0] _
      | d0 == d1 && d1 /= d2 = Seen
      | otherwise = Unseen
    nextDoubledStatus Unseen Part2 (d3 : d2 : d1 : d0 : _) lowerTl
      | d0 /= d1 && d1 == d2 && d2 /= d3 = Seen
      | d1 /= d2 && d2 == d3 && null lowerTl = Seen
      | otherwise = Unseen

    increasingOk [] = undefined
    increasingOk [_] = Just ()
    increasingOk (d1 : d0 : _)
      | d1 >= d0 = Just ()
      | otherwise = Nothing

parseLimit :: T.Text -> [Int]
parseLimit = map digitToInt . T.unpack

parseInput :: T.Text -> ([Int], [Int])
parseInput s =
  case T.splitOn "-" $ T.strip s of
    [left, right] -> (parseLimit left, parseLimit right)
    _ -> error (printf "can't parse %s" s)

doPart1 :: T.Text -> IO ()
doPart1 input = do
  let (lower, upper) = parseInput input
  printf "part1, count: %d\n" (countPass Part1 lower upper)

doPart2 :: T.Text -> IO ()
doPart2 input = do
  let (lower, upper) = parseInput input
  printf "part2, count: %d\n" (countPass Part2 lower upper)

run :: IO ()
run = do
  input <- TIO.getContents
  doPart1 input
  doPart2 input
