module Maze.Maze where

import Data.List
import Data.List.Split
import System.Random (randomRIO)
import Data.Array.IO
import Control.Monad

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j  <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs

data Cell = Cell { getX :: Int
                 , getY :: Int
                 , getType :: Char
                 } deriving (Show)

type Board      = [Cell]
type Candidates = [Cell]
type Neighbors  = [Cell]

genBoard :: Int -> Int -> Board
genBoard w h = [Cell x y 'X'
               | y <- [1..h]
               , x <- [1..w]]

cellToTuple :: Cell -> ((Int, Int), Char)
cellToTuple (Cell x y t) = ((x, y), t)

printMaze :: Board -> IO ()
printMaze board =
        let l = last board
            w = getX l
        in putStr . unlines
                  . map (intersperse ' ')
                  . chunksOf w
                  . map getType
                  $ board

updateInBoard :: (Char -> Char) -> Board -> Cell -> Board
updateInBoard _ [] _         = []
updateInBoard f (c'@(Cell x' y' t):cells) c@(Cell x y _)
        | x == x' && y == y' = Cell x y (f t) : cells
        | otherwise          = c' : updateInBoard f cells c

floorIt :: Board -> Cell -> Board
floorIt = updateInBoard (const '_')

randNth :: [a] -> IO a
randNth l = randomRIO (0, length l - 1)
        >>= return . (l !!)

getRadialNeighbors :: Board -> Cell -> Neighbors
getRadialNeighbors b (Cell x y _)
        = [Cell x' y' (getT x' y')
            | dx <- [-1..1], dy <- [-1..1],
            let x' = (x+dx), let y' = (y+dy)]
        where getT :: Int -> Int -> Char
              getT i j = getType (b' !! (i-1) !! (j-1))
              b' = chunksOf (truncate
                            . sqrt
                            . fromIntegral
                            . length $ b) b

onSameAxis :: Cell -> Cell -> Cell -> Bool
onSameAxis orig@(Cell x y _) trgt@(Cell i j _) cell@(Cell x' y' _)
        | y == j    = x == x'
        | x == i    = y == y'
        | otherwise = error "onSameAxis: orig & trgt must be adj"

getNeighbors :: Board -> Cell -> Cell -> Neighbors
getNeighbors b orig@(Cell x y _) trgt@(Cell i j _)
        = filter (not . onSameAxis orig trgt)
                 $ getRadialNeighbors b trgt

getNeighborsOfType :: Board -> Cell -> Cell -> Char -> Neighbors
getNeighborsOfType b orig trgt t = filter ((t ==) . getType)
                                          $ getNeighbors b orig trgt

getWallNeighbors :: Board -> Cell -> Cell -> Neighbors
getWallNeighbors b o t = getNeighborsOfType b o t 'X'

getAValidNeighbor :: Board -> Cell -> Neighbors -> Cell
getAValidNeighbor _ _ [] = error "derp"
getAValidNeighbor board mark neighbors =
        if 5 == cWallCount
            then c
            else getAValidNeighbor board mark cs
        where cWallCount = 5 --length $ getWallNeighbors board mark c
              (c:cs) = neighbors
