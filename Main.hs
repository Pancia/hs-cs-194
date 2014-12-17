import Maze.Maze

main :: IO ()
--main = randNth board
--            >>= return . (updateInBoard (const '_') board)
--            >>= printMaze
--            >>  putStr "\n"
main = shuffle (getNeighbors b c1 c2) >>=
       return . (getAValidNeighbor b (Cell 3 3 '?')) >>=
       printCell
       where b = genBoard 5 5
             c1 = (Cell 3 3 '*')
             c2 = (Cell 3 4 '*')
             printCell :: Cell -> IO ()
             printCell = print
