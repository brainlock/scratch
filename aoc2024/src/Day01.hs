{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.RawString.QQ
import Data.Function ((&))
import Data.List (sort, foldl')
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as M

parseInput :: String -> [(Int, Int)]
parseInput s = map parseLine (lines s)
    where
        parseLine l = case words l of
            [a, b] -> (read a, read b)
            _ -> error $ "malformed input line: " ++ l

ingestInput :: String -> ([Int], [Int])
ingestInput s = parseInput s & unzip

totalDistance :: ([Int], [Int]) -> Int
totalDistance input = bimap sort sort input
    & uncurry zip
    & map (\(a, b) -> abs $ a - b)
    & sum

similarityScore :: ([Int], [Int]) -> Int
similarityScore (leftList, rightList) = map (\x -> x * occurrences x) leftList & sum
    where
        counts = foldl' (\acc x -> M.insert x (1 + M.findWithDefault 0 x acc) acc) M.empty rightList
        occurrences :: Int -> Int
        occurrences x = M.findWithDefault 0 x counts

main :: IO ()
main = do
    let testInput = [r|3   4
        4   3
        2   5
        1   3
        3   9
        3   3|] & ingestInput
    input <- readFile "inputs/day01" <&> ingestInput
    putStrLn $ "test 1: " ++ show (totalDistance testInput)
    putStrLn $ "solution 1:" ++ show (totalDistance input)
    putStrLn $ "test 2: " ++ show (similarityScore testInput)
    putStrLn $ "solution 2:" ++ show (similarityScore input)
