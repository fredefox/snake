type Point = (Int, Int)

data World = World
    { size ::  Point
    , food ::  Point
    , snake :: [Point]
    }

main = game initialWorld >>= game

initialWorld :: World
initialWorld = World
    { size = (10, 10)
    , food = ( 5,  5)
    , snake = [( 0, 0), (0, 1), (0, 2)]
    }

game :: World -> IO World
game w = do
    printWorld w
    mDir <- getInput
    maybe w (modifyWorld w) mDir

printWorld :: World -> IO ()
printWorld = error "printWorld: Not implemented"

getInput :: IO Direction
getInput = getChar >>= \ x -> case x of
    'w' -> Just Up
    'a' -> Just Left
    's' -> Just Down
    'd' -> Just Right

data Direction = Up | Left | Down | Right

modifyWorld :: World -> Direction -> World
modifyWorld = error "modifyWorld: Not implemented"
