module Main where

import Prelude hiding (Left, Right)
import System.Random (randomRIO)
import Data.List ((\\))
import Control.Monad
import System.Exit

type Point = (Int, Int)

data World = World
    { size ::  Point
    , food ::  Point
    , snake :: [Point]
    }

main :: IO ()
main = go initialWorld where
    go w = game w >>= go

initialWorld :: World
initialWorld = World
    { size = (3, 3)
    , food = (2, 2)
    , snake = [(x, 0) | x <- [0..2]]
    }

game :: World -> IO World
game w = do
    printWorld w
    mDir <- getInput
    maybe (return w) (modifyWorld w) mDir

printWorld :: World -> IO ()
printWorld = putStrLn . worldToStr

rowToStr   :: World -> Int -> String
rowToStr w j = [chrAt (x, j) | x <- [0..xmax-1]] where
    (xmax, _) = size w
    chrAt p
        | p `elem` snake w = '*'
        | p     ==  food w = '%'
        | otherwise        = ' '


worldToStr :: World -> String
worldToStr w = unlines [rowToStr w y | y <- [0..ymax-1]] where
    (_, ymax) = size w

getInput :: IO (Maybe Direction)
getInput = match `fmap` getChar where
    match x = case x of
        'w' -> Just Up
        'a' -> Just Left
        's' -> Just Down
        'd' -> Just Right
        _   -> Nothing

data Direction = Up | Left | Down | Right

modifyWorld :: World -> Direction -> IO World
modifyWorld w d = do
    newWorld <- getNewWorld
    blarg newWorld where
    blarg newWorld
        | worldFull newWorld = gameOver
        | foodEaten = (\f -> newWorld { food = f })
            `fmap` (choice . emptySpots) newWorld
        | otherwise = return newWorld
    worldFull = (== 0) . length . emptySpots
    getNewWorld = if head movedSnake `elem` emptySpots w
        then return $ w { snake = movedSnake }
        else gameOver
    movedSnake = newHead : newTail
    newTail = if foodEaten then theSnake else init theSnake
    newHead = modIt $ case d of
        Up    -> (  shx, shy-1)
        Left  -> (shx-1,   shy)
        Down  -> (  shx, shy+1)
        Right -> (shx+1,   shy)
    modIt (x, y) = (x `mod` xmax, y `mod` ymax)
    (shx, shy) = head theSnake
    (xmax, ymax) = size w
    theSnake = snake w
    foodEaten = newHead == food w

gameOver :: IO a
gameOver = putStrLn "Game over" >> exitSuccess

emptySpots :: World -> [Point]
emptySpots w = allPoints \\ snake w where
    allPoints = [(x, y) | x <- [0..xmax-1], y <- [0..ymax-1]]
    (xmax, ymax) = size w

choice :: [a] -> IO a
choice xs = (xs !!) `liftM` randomRIO (0, length xs - 1)
