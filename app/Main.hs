module Main where

import Control.Monad
import System.Environment
import System.IO

main = do
  args <- getArgs
  let fileName = head args
  let qtyIterations = args !! 1
  tableInput <- readFile fileName
  let table = lines tableInput
  printTable table

-- A entrada consiste num txt do seguinte formato:
--
-- v........z.......
-- v........z.......
-- .........v.......
-- .....v...........
-- ............v....
-- ........v........
--
-- Em que:
-- "." : são as células mortas
-- "v" : as células vivas
-- "z" : as células zumbis

printTable :: [String] -> IO()
printTable = mapM_ putStrLn


