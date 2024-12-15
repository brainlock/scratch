{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.RawString.QQ (r)
import Data.List.Split (splitOn)
import Data.Function ((&))
import Control.Monad (liftM2)
import Data.Functor ((<&>))

testInput :: String
testInput = [r|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|]

data Equation = Equation {
    elems :: [Int],
    result :: Int
} deriving (Show)

type Input = [Equation]

data Op = Add | Mul | Concat deriving (Show)
applyOp :: (Num a, Read a, Show a, Integral a) => Op -> a -> a -> a
applyOp Add = (+)
applyOp Mul = (*)
applyOp Concat = \a b -> a * f b + b
    where
        f x = if div x 10 == 0 then 10 else 10 * f (div x 10)

-- >>> parse "123: 2 3 4 4"
-- [Equation {elems = [2,3,4,4], result = 123}]
parse :: String -> Input
parse s = lines s & map parseLine
    where
        parseLine :: String -> Equation
        parseLine l = Equation {
            elems = firstSplit l & parseLineElems,
            result = firstSplit l & parseLineExp
        }
        firstSplit x = splitOn ":" x & filter (not . null)
        parseLineExp :: [String] -> Int
        parseLineExp (x:_) = read x
        parseLineExp _ = error "invalid input while reading result"
        parseLineElems :: [String] -> [Int]
        parseLineElems [_, l] = splitOn " " l & filter (not . null) & map read
        parseLineElems _ = error "invalid input while reading elems"

data Result = Possible [Op] | Impossible deriving (Show)

-- >>> solveEq (Equation { result=10, elems=[2, 3, 4] })
-- Possible [Mul,Add]
solveEq :: [Op] -> Equation -> Result
solveEq possibleOps eq = case solveEq' eq (opsCombinations possibleOps $ length (elems eq) - 1) Nothing of
    Just x -> x
    Nothing -> error "invalid, should always get to a result"

solveEq' :: Equation -> [[Op]] -> Maybe Result -> Maybe Result
solveEq' _ _ (Just x) = Just x
solveEq' _ [] _ = Just Impossible
solveEq' eq (ops:otherCombinations) res = case calculate (elems eq) ops `compare` result eq of
    EQ -> Just $ Possible ops
    _ -> solveEq' eq otherCombinations res

-- >>> calculate [2, 3, 4] [Mul, Add]
-- 10
calculate :: (Num a, Read a, Show a, Integral a) => [a] -> [Op] -> a
calculate [] _ = error "invalid state, empty eq"
calculate [a] _ = a
calculate (_:_:_) [] = error "invalid state, not enough ops (wrong length?)"
calculate (a:b:rest) (op:ops) = calculate (applyOp op a b:rest) ops

-- looks like I could have just written `sequence (replicate n ops)`…
-- >>> sequence (replicate 3 [Add, Mul])
-- >>> opsCombinations [Add, Mul] 3
-- [[Add,Add,Add],[Add,Add,Mul],[Add,Mul,Add],[Add,Mul,Mul],[Mul,Add,Add],[Mul,Add,Mul],[Mul,Mul,Add],[Mul,Mul,Mul]]
-- [[Add,Add,Add],[Add,Add,Mul],[Add,Mul,Add],[Add,Mul,Mul],[Mul,Add,Add],[Mul,Add,Mul],[Mul,Mul,Add],[Mul,Mul,Mul]]
opsCombinations :: [a] -> Int -> [[a]]
opsCombinations ops n = last $ take (n+1) $ iterate f [[]]
    where
        f = liftM2 (:) ops

solve :: [Op] -> Input -> Int
solve possibleOps eqs = filter (isPossible . solveEq possibleOps) eqs
    & map result
    & sum
    where
    isPossible (Possible _) = True
    isPossible _ = False

solve1 :: Input -> Int
solve1 = solve [Add, Mul]

solve2 :: Input -> Int
solve2 = solve [Add, Mul, Concat]

main :: IO ()
main = do
    let t = parse testInput
    putStrLn $ "test1: " ++ show (solve1 t)

    input <- readFile "inputs/day07" <&> parse
    putStrLn $ "solution1: " ++ show (solve1 input)

    putStrLn $ "test2: " ++ show (solve2 t)
    putStrLn $ "solution2: " ++ show (solve2 input)
