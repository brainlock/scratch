{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Text.RawString.QQ (r)
import qualified Data.Array as A
import Data.Function ((&))
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Foldable (find)
import GHC.Stack (HasCallStack)
import Data.Functor ((<&>))

testInput :: String
testInput = [r|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|]

type Input = (LabMap, Guard)

data Cell = Free | Obstructed
    deriving (Eq, Show)

type Idx = (Int, Int)
type LabMap = A.Array Idx Cell

data Completion = Loop | Out
    deriving (Show, Eq)

data Direction = DirUp | DirDown | DirLeft | DirRight
    deriving (Eq, Ord)

instance Show Direction
    where
        show DirUp = "^"
        show DirDown = "v"
        show DirLeft = "<"
        show DirRight = ">"

instance Read Direction where
  readsPrec :: Int -> ReadS Direction
  readsPrec _ "^" = [(DirUp, "")]
  readsPrec _ "v" = [(DirDown, "")]
  readsPrec _ "<" = [(DirLeft, "")]
  readsPrec _ ">" = [(DirRight, "")]
  readsPrec _ _ = []

data Guard = Guard Idx Direction
    deriving (Show, Eq, Ord)

guardPosition :: Guard -> Idx
guardPosition (Guard p _) = p

parse :: HasCallStack => String -> (LabMap, Guard)
parse inputStr = (fmap parseCell inputArray, Guard findGuardPosition findGuardDirection)
    where
        inputArray :: A.Array Idx Char
        inputArray = lines inputStr & concat & A.listArray ((0, 0), (max_i, max_j))
        max_i = length (lines inputStr) - 1
        max_j = length (head (lines inputStr)) - 1
        parseCell :: Char -> Cell
        parseCell '#' = Obstructed
        parseCell '.' = Free
        parseCell '^' = Free
        parseCell '>' = Free
        parseCell '<' = Free
        parseCell 'v' = Free
        parseCell _ = error "invalid input"
        findGuard :: (Idx, Char)
        findGuard = find (\(_, c) -> not (null (reads [c] :: [(Direction, String)]))) (A.assocs inputArray)
            & fromJust
        findGuardPosition = findGuard & fst
        findGuardDirection = findGuard & snd & \x -> read [x]

idxAdd :: Idx -> Idx -> Idx
idxAdd a b = (fst a + fst b, snd a + snd b)

inRange :: LabMap -> Idx -> Bool
inRange = A.inRange . A.bounds

step :: LabMap -> Guard -> Maybe Guard
step lab (Guard pos direction) = if inRange lab (guardPosition nextGuard) then Just nextGuard else Nothing
    where
        nextCell = idxAdd pos (move direction)
        nextGuard =
            if inRange lab nextCell
            then case lab A.! nextCell of
                Free -> Guard nextCell direction
                Obstructed -> Guard pos $ turn direction
            else Guard nextCell direction

turn :: Direction -> Direction
turn DirUp = DirRight
turn DirDown = DirLeft
turn DirRight = DirDown
turn DirLeft = DirUp

move :: Direction -> Idx
move DirUp = (-1, 0)
move DirDown = (1, 0)
move DirRight = (0, 1)
move DirLeft = (0, -1)

run :: Input -> [Guard]
run (labMap, guard) =
    let next = step labMap guard
    in
        case next of
            Just s -> guard:run (labMap, s)
            Nothing -> [guard]

runToCompletion :: LabMap -> S.Set Guard -> Guard -> Completion
runToCompletion lab seen (Guard pos direction)
    | inRange lab nextCell =
        let next = evalNextCell nextCell in
            if next `S.member` seen then
                Loop
            else
                runToCompletion lab (S.insert (Guard pos direction) seen) next
    | otherwise = Out
    where
        nextCell = idxAdd pos (move direction)
        evalNextCell c = case lab A.! c of
            Free -> Guard nextCell direction
            Obstructed -> Guard pos $ turn direction


solve1 :: Input -> Int
solve1 input = run input & map guardPosition & S.fromList & length

solve2 :: Input -> Int
solve2 (lab, guard) =
    [(c, replaceCell lab c) | c <- emptyCells]
    & map (\(c, lab') -> (c, runToCompletion lab' S.empty guard))
    & filter (\(_, end) -> end == Loop)
    & map fst
    & countUnique
    where
        emptyCells = A.assocs lab & filter (\(_, c) -> c == Free) & map fst
        replaceCell l c = l A.// [(c, Obstructed)]
        countUnique = length . S.fromList

main :: IO ()
main = do
    let t = parse testInput
    putStrLn $ "test input 1: " ++ show (solve1 t)

    input <- readFile "inputs/day06" <&> parse
    putStrLn $ "solution 1: " ++ show (solve1 input)

    putStrLn $ "test input 2: " ++ show (solve2 t)
    putStrLn $ "solution 2: " ++ show (solve2 input)
