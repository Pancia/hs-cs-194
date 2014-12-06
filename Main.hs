import Data.List
import Data.List.Split
import System.Random (randomRIO)

data Cell = Cell { x :: Int
                 , y :: Int
                 , t :: Char
                 } deriving (Show)

type Board = [Cell]

genBoard :: Int -> Int -> Board
genBoard w h = [Cell x y 'X'
               | y <- [1..h]
               , x <- [1..w]]

printMaze :: Board -> IO ()
printMaze board =
        let l = last board
            w = x l
        in putStr . unlines
                  . map (intersperse ' ')
                  . chunksOf w
                  . map t
                  $ board

updateInBoard :: (Char -> Char) -> Int -> Int -> Board -> Board
updateInBoard _ _ _ []       = []
updateInBoard f x y (c@(Cell x' y' t):cells)
        | x == x' && y == y' = Cell x y (f t) : cells
        | otherwise          = c : updateInBoard f x y cells

pick :: Board -> IO Cell
pick cells = randomRIO (0, length cells - 1)
             >>= return . (cells !!)


