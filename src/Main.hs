{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (Left, Right)
import System.Random (randomRIO)
import Data.List ((\\))
import Control.Monad
import System.Exit
import UI.NCurses
import Control.Monad
import Control.Monad.IO.Class

type Point = (Int, Int)

data World = World
    { size ::  Point
    , food ::  Point
    , snake :: [Point]
    }

main :: IO ()
main = runCurses play

-- Time between refresh in ms.
speed :: Integer
speed = 100

play :: Curses ()
play = do
  setEcho False
  win <- defaultWindow
  loop win initialWorld Down
  where
    loop :: Window -> World -> Direction -> Curses ()
    loop win world dir = do
      updateWindow win $ do
        moveCursor 0 0
        printWorld world
      render
      mDir <- getDirection <$> getEvent win (Just speed)
      let dir' = maybe dir id mDir
      w <- modifyWorld world dir'
      loop win w dir'

getDirection :: Maybe Event -> Maybe Direction
getDirection ev = ev >>= onlyCharEv >>= match
  where
    onlyCharEv = \case
      EventCharacter c -> Just c
      _ -> Nothing
    match x = case x of
        'w' -> Just Up
        'a' -> Just Left
        's' -> Just Down
        'd' -> Just Right
        _   -> Nothing

initialWorld :: World
initialWorld = World
    { size = (10, 10)
    , food = (2, 2)
    , snake = [(x, 0) | x <- [0..2]]
    }

printWorld :: World -> Update ()
printWorld = drawString . worldToStr

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

data Direction = Up | Left | Down | Right

modifyWorld :: MonadIO m => World -> Direction -> m World
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

gameOver :: MonadIO m => m a
gameOver = liftIO $ putStrLn "Game over" >> exitSuccess

emptySpots :: World -> [Point]
emptySpots w = allPoints \\ snake w where
    allPoints = [(x, y) | x <- [0..xmax-1], y <- [0..ymax-1]]
    (xmax, ymax) = size w

choice :: MonadIO m => [a] -> m a
choice xs = liftIO $ (xs !!) `liftM` randomRIO (0, length xs - 1)
