{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.RawString.QQ
import Data.Function ((&))
import Data.List (foldl')
import Data.Functor ((<&>))

testInput :: [Report]
testInput = [r|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|] & parseInput

type Level = Int
type Report = [Level]

isMonotonic :: Report -> Bool
isMonotonic report =
    all (>= 0) (increments report) || all (<= 0) (increments report)

increments :: Report -> [Int]
increments [] = []
increments (x:y:rest) = (x - y) : increments (y:rest)
increments (_:rest) = increments rest

isSafe :: Report -> Bool
isSafe report =
       isMonotonic report
    && all (\x -> x >= 1 && x <= 3) diffs
    where
        diffs = map abs $ increments report

parseInput :: String -> [Report]
parseInput s = map parseLine (lines s)
    where
        parseLine :: String -> [Level]
        parseLine l = map read $ words l

count' :: Foldable t => (a -> Bool) -> t a -> Int
count' predicate = foldl' (\acc x -> if predicate x then acc + 1 else acc) 0

solve :: [Report] -> Int
solve = count' isSafe

isSafeWithDamper :: Report -> Bool
isSafeWithDamper report = any isSafe $ modifiedLevels report
    where
        modifiedLevels :: Report -> [Report]
        modifiedLevels [] = []
        modifiedLevels (x:xs) = xs : map (x:) (modifiedLevels xs)

solve2 :: [Report] -> Int
solve2 = count' $ \x -> isSafe x || isSafeWithDamper x

main :: IO ()
main = do
    input <- readFile "inputs/day02" <&> parseInput
    putStrLn $ "test 1: " ++ show (solve testInput)
    putStrLn $ "solution 1:" ++ show (solve input)
    putStrLn $ "test 2: " ++ show (solve2 testInput)
    putStrLn $ "solution 2:" ++ show (solve2 input)
