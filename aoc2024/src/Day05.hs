{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Maybe
import Text.RawString.QQ (r)
import Data.Function ((&))
import qualified Data.Map as M
import Data.Map (Map)
import Data.List.Split (splitOn)
import Data.Foldable (Foldable(foldl'))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Functor ((<&>))
import Data.List (sortBy)

testInput :: String
testInput = [r|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|]

type PageN = Int
type PrintJob = [PageN]
type Dependencies = Map PageN (Set PageN)

data Input = Input {
    dependencies :: Dependencies,
    printJobs :: [PrintJob]
} deriving (Show)

parse :: String -> Input
parse inputString = Input { dependencies=d, printJobs=pjs }
    where
        d = parseDeps (head parts)
        pjs = parseJobs (parts !! 1)
        parts = splitOn "\n\n" inputString

parseDeps :: String -> Dependencies
parseDeps s = lines s
    & map readDepLine
    & M.fromListWith S.union

readDepLine :: String -> (Int, Set Int)
readDepLine s = splitOn "|" s & \parts -> (read $ parts !! 1, S.singleton (read $ head parts))

parseJobs :: String -> [PrintJob]
parseJobs s = lines s & map (map read . splitOn ",")

isCorrectOrder :: Input -> PrintJob -> Bool
isCorrectOrder input pages = foldl' f (True, S.empty) pages & fst
    where
        deps = subGraph (dependencies input) (S.fromList pages)
        subGraph depsMap problemPages = depsMap
            & M.filterWithKey (\k _ -> k `S.member` problemPages)
            & M.map (S.filter (`S.member` problemPages))
        f (isCorrect, seen) page = (isCorrect && verifyPage page (S.insert page seen), S.insert page seen)
        verifyPage page seen = all (`S.member` seen) (depsOfPage page)
        depsOfPage page = Data.Maybe.fromMaybe S.empty $ M.lookup page deps

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

solve1 :: Input -> Int
solve1 input = sum $ map middle correctJobs
    where
        correctJobs = filter (isCorrectOrder input) (printJobs input)

solve2 :: Input -> Int
solve2 input = sum $ map middle fixedJobs
    where
        fixedJobs = map (sortPages input) incorrectJobs
        incorrectJobs = filter (not . isCorrectOrder input) (printJobs input)

sortPages :: Input -> PrintJob -> PrintJob
sortPages input = sortBy comp
    where
        comp a b
          | a `isDepOf` b = LT
          | b `isDepOf` a = GT
          | otherwise = EQ
        isDepOf p1 p2 = p1 `S.member` Data.Maybe.fromMaybe S.empty (M.lookup p2 (dependencies input))

test :: String -> Bool -> IO ()
test name f = do
    putStr $ if f then "✅ " else "❌ "
    putStrLn name

main :: IO ()
main = do
    --putStrLn $ "deps: " ++ show (dependencies t)
    --putStrLn $ "jobs: " ++ show (printJobs t)
    --let expected = [True, True, True, False, False, False]
    --forM_ (zip (printJobs t) expected) $ \(job, result) -> do
        --test (show job) $ result == isCorrectOrder t job

    let t = testInput & parse
    putStrLn $ "test input 1: " ++ show (solve1 t)
    input <- readFile "inputs/day05" <&> parse
    putStrLn $ "solution 1: " ++ show (solve1 input)

    putStrLn $ "test input 2: " ++ show (solve2 t)
    putStrLn $ "solution 2: " ++ show (solve2 input)
