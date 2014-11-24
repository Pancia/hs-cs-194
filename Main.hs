import Data.List
import Data.List.Split

data Cell = Cell { x :: Int
                 , y :: Int
                 , t :: Char
                 } deriving (Show)

genBoard w h = [(Cell x y 'X')
               | y <- [1..h], x <- [1..w]]

printMaze board =
        let l = last board
            w = x l
        in putStr . unlines
                  . (map (intersperse ' '))
                  . (chunksOf w)
                  . (map t)
                  $ board

