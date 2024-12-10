{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

module Main where

import Text.RawString.QQ (r)
import qualified Data.Array as A
import Data.Function ((&))
import Data.Functor ((<&>))

testInput :: String
testInput = [r|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|]

type Idx = (Int, Int)
type Input = A.Array Idx Char

parse :: String -> Input
parse a = A.listArray ((0, 0), (max_i, max_j)) $ concat rows
    where
        rows = lines a
        max_j = length rows - 1
        max_i = length (head rows) - 1

inRange :: Input -> Idx -> Bool
inRange = A.inRange . A.bounds

idxAdd :: Idx -> Idx -> Idx
idxAdd a b = (fst a + fst b, snd a + snd b)

genSeqs :: Input -> [[Idx]]
genSeqs arr = concat
    [genWithMove 4 step arr | step <-
        [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1)]]

genWithMove :: Int -> Idx -> Input -> [[Idx]]
genWithMove n step arr = map (take n . seqFromIdx) (A.indices arr) & filter (\x -> length x == n)
    where
        seqFromIdx :: Idx -> [Idx]
        seqFromIdx idx = if inRange arr idx then idx:seqFromIdx (idxAdd idx step) else []

getStr :: Input -> [Idx] -> String
getStr arr = map (arr A.!)

solve1 :: Input -> Int
solve1 arr = genSeqs arr & map (getStr arr) & filter ("XMAS" ==) & length

solve2 :: Input -> Int
solve2 arr = A.elems counts & filter (== 2) & length
    where
        diags = concatMap findMas [genWithMove 3 step arr | step <- [(1, 1), (-1, -1), (-1, 1), (1, -1)]]
        findMas = filter (\idxs -> "MAS" == map (arr A.!) idxs)
        counts = A.accumArray (+) 0 (A.bounds arr) (concatMap getScore diags)
        getScore :: [Idx] -> [(Idx, Int)]
        getScore idxs = enumerate idxs & map (\(pos, idx) -> (idx, score pos))
        enumerate = zip ([0..]::[Int])
        score pos = if pos == 1 then 1 else 0

main :: IO ()
main = do
    let t = parse testInput
    putStrLn $ "test input 1: " ++ show (solve1 t)
    input <- readFile "inputs/day04" <&> parse
    putStrLn $ "solution 1: " ++ show (solve1 input)
    putStrLn $ "test input 2: " ++ show (solve2 t)
    putStrLn $ "solution 2: " ++ show (solve2 input)
