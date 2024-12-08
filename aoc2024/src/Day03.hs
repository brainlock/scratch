module Main where
import Data.Function ((&))

data Exp = Mul Int Int | Foo
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

inject :: String -> S
inject s = ([], Init, s)

eval :: Exp -> Int
eval (Mul a b) = a * b
eval _ = undefined

solve1 :: [Exp] -> Int
solve1 muls = map eval muls & sum

main :: IO ()
main = do
    let test1 = parse testInput & solve1
    putStrLn $ "test1: " ++ show test1

    input <- readFile "inputs/day03"
    let result1 = parse input & solve1
    putStrLn $ "result1: " ++ show result1
