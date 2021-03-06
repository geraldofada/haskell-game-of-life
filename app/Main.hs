module Main where

import Control.Monad
import System.Environment
import System.IO

-- A entrada consiste num txt do seguinte formato:
--
-- v........z........
-- v........z........
-- .........v........
-- .....v............
-- ............v.....
-- ........v.........
--
-- v........z........
-- v........z........
-- v........z........
--
-- Em que:
-- "." : são as células mortas
-- "v" : as células vivas
-- "z" : as células zumbis

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  let qtyIterations = read (args !! 1) :: Int
  tableInput <- readFile fileName
  let table = lines tableInput
  let (finalTable, iterations) = createFinalTable table qtyIterations 1
  print iterations
  printTable finalTable

type Cell = Char

type Table = [[Cell]]

type TableLine = [Cell]

type Neighbors = [Cell]

type PosX = Int

type PosY = Int

printTable :: Table -> IO ()
printTable = mapM_ putStrLn

-- Dado o estado atual da tabela e as posições de uma célula
-- retorna uma lista com seus vizinhos
getNeighborsCells :: Table -> PosX -> PosY -> Neighbors
getNeighborsCells table i j = [table !! x !! y | x <- [i - 1 .. i + 1], x >= 0, x < length table, y <- [j - 1 .. j + 1], y >= 0, y < length (table !! i), (x, y) /= (i, j)]

-- Dado uma lista de vizinhos e um tipo de célula retorna a quantidade
-- que ela aparece na lista de vizinhos
countCellType :: Neighbors -> Cell -> Int 
countCellType [] cellToFind = 0
countCellType neighbors cellToFind = length res
    where res = [res | res <- neighbors, res == cellToFind]

-- Dado um estado da tabela, posições de uma célula
-- retorna um novo estado da célula na posição passada
processCell :: Table -> PosX -> PosY -> Cell
processCell table i j =
  let
    neighbors = getNeighborsCells table i j
    deadCount = countCellType neighbors '.'
    aliveCount = countCellType neighbors 'v'
    zombieCount = countCellType neighbors 'z' 
    currentCell = table !! i !! j
  in
  if currentCell == '.' && aliveCount == 3
     then 'v'
  else if currentCell == 'v' && zombieCount >= 2
     then 'z'
  else if currentCell == 'v' && aliveCount < 2 && zombieCount < 2
     then '.'
  else if currentCell == 'v' && aliveCount > 3 && zombieCount == 0
     then '.'
  else if currentCell == 'z' && aliveCount == 0
     then '.'
  else
      currentCell

-- Dado um estado da tabela e uma posição (linha da tabela)
-- retorna uma linha da tabela processada
processLine :: Table -> PosX -> TableLine
processLine table x = [processCell table x y | y <- [0..(length (table !! x) - 1)]]

-- Dado um estado da tabela retona uma nova tabela no próximo estado
processTable :: Table -> Table
processTable table = [processLine table x | x <- [0..length table - 1]]

-- Dado um estado da tabela, quantidade máxima de iterações e uma iteração inicial
-- retorna uma tupla com a tabela final e a quantidade de iterações do jogo da vida
-- passadas
createFinalTable :: Table -> Int -> Int -> (Table, Int)
createFinalTable table maxIterations i =
  let
    newTable = processTable table
  in
    if newTable == table
       then (table, i-1)
    else if maxIterations == i
       then (newTable, i)
    else if maxIterations < 1
       then (table, 0)
    else
       createFinalTable newTable maxIterations (i+1)
