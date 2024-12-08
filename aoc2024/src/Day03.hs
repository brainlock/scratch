module Main where
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Foldable (Foldable(foldl'))

data Exp = Mul Int Int
    | Do
    | Dont
    deriving (Show)

data State =
    Init
    | CtMul
    | CtMul1 Int
    | CtMul2 Int Int
    deriving (Show)

type S = ([Exp], State, String)

parseStep :: S -> S
parseStep (p, s, "") = (p, s, "")
parseStep (p, Init, 'd':'o':'(':')':rest) = (p ++ [Do], Init, rest)
parseStep (p, Init, 'd':'o':'n':'\'':'t':'(':')':rest) = (p ++ [Dont], Init, rest)
parseStep (p, Init, 'm':'u':'l':'(':rest) = (p, CtMul, rest)
parseStep (p, CtMul, s) = case reads s of
    [(n, rest)] -> (p, CtMul1 n, rest)
    _ -> (p, Init, s)
parseStep (p, CtMul1 arg1, ',':s) = case reads s of
    [(n, rest)] -> (p, CtMul2 arg1 n, rest)
    _ -> (p, Init, s)
parseStep (p, CtMul2 arg1 arg2, ')':s) = (p ++ [Mul arg1 arg2], Init, s)
parseStep (p, _, _:rest) = (p, Init, rest)

trace :: (S, [S]) -> (S, [S])
trace (state, t) = case parseStep state of
    (p, s, "") -> ((p, s, ""), t)
    next -> trace (next, t ++ [next])

parse :: String -> [Exp]
parse str = parse' $ inject str
    where
        parse' state = case parseStep state of
            (p, _, "") -> p
            next -> parse' next

testInput :: String
testInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

testInput2 :: String
testInput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

inject :: String -> S
inject s = ([], Init, s)

eval1 :: Exp -> Maybe Int
eval1 (Mul a b) = Just $ a * b
eval1 _ = Nothing

solve1 :: [Exp] -> Int
solve1 muls = mapMaybe eval1 muls & sum

data MulState = Enabled | Disabled

solve2 :: [Exp] -> Int
solve2 prog = foldl' eval2 (Enabled, 0) prog & snd
    where
        eval2 (_, total) Do = (Enabled, total)
        eval2 (_, total) Dont = (Disabled, total)
        eval2 (Enabled, total) (Mul a b) = (Enabled, total + (a * b))
        eval2 (Disabled, total) (Mul _ _) = (Disabled, total)

main :: IO ()
main = do
    let test1 = parse testInput & solve1
    putStrLn $ "test1: " ++ show test1

    input <- readFile "inputs/day03"
    let result1 = parse input & solve1
    putStrLn $ "result1: " ++ show result1

    let test2 = parse testInput2 & solve2
    print test2

    let result2 = parse input & solve2
    putStrLn $ "result2: " ++ show result2
