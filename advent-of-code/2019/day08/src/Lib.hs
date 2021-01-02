{-# LANGUAGE OverloadedStrings #-}

module Lib (run) where

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Text.Printf (printf)

textToList :: T.Text -> [Char]
textToList t = case T.uncons t of
  Just (hd, tl) -> hd : textToList tl
  Nothing -> []

readRow :: T.Text -> V.Vector Int
readRow rowText = V.fromList $ map digitToInt $ textToList rowText

readLayer :: Int -> T.Text -> V.Vector (V.Vector Int)
readLayer rowLength layerText = V.fromList $ map readRow $ T.chunksOf rowLength layerText

readInput :: Int -> Int -> T.Text -> V.Vector (V.Vector (V.Vector Int))
readInput rows cols input =
  V.fromList $ map (readLayer cols) $ T.chunksOf (rows * cols) $ T.strip input

countCond :: (Int -> Bool) -> V.Vector (V.Vector Int) -> Int
countCond cond layer = V.sum $ V.map countCondInRow layer
  where
    countCondInRow row = V.length $ V.filter cond row

render :: V.Vector (V.Vector (V.Vector Int)) -> V.Vector (V.Vector Int)
render mtx =
  V.generate rows $ \r ->
    V.generate cols $ \c ->
      fromMaybe 2 $ V.find (/= 2) $ drill r c
  where
    rows = V.length $ mtx V.! 0
    cols = V.length $ mtx V.! 0 V.! 0
    drill r c = V.map (\layer -> layer V.! r V.! c) mtx

imageToString :: V.Vector (V.Vector Int) -> T.Text
imageToString = T.intercalate "\n" . map rowToString . V.toList
  where
    rowToString = T.concat . map (T.pack . show) . V.toList

solvePart1 :: T.Text -> IO ()
solvePart1 input = do
  let mtx = readInput 6 25 input
      -- let mtx = readInput 2 3 input
      bestLayer = V.minimumBy (comparing $ countCond (== 0)) mtx
      countOnes = countCond (== 1) bestLayer
      countTwos = countCond (== 2) bestLayer
      answer = countOnes * countTwos
  printf "part1, ones: %d, twos: %d, answer: %d\n" countOnes countTwos answer

solvePart2 :: T.Text -> IO ()
solvePart2 input = do
  let mtx = readInput 6 25 input
      rendered = render mtx
      renderedText = imageToString rendered
  printf "part2:\n%s\n" (T.unpack $ T.replace "0" " " renderedText)

run :: IO ()
run = do
  input <- TIO.getContents
  solvePart1 input
  solvePart2 input
