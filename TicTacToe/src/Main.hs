module Main where

import Game

--printGrid :: Board -> [IO ()]
--printGrid bd = [print line | line <- bd]

main = do
  putStrLn "Tic Tac Toe"
  putStrLn "Choose the board size"
  nl <- getLine
  let n = (read nl :: Int) 
      board = emptyBoard n
  print board
  gameLoop board Cross
  
changePlayer :: Mark -> Mark
changePlayer Cross = Nought
changePlayer Nought = Cross 

gameLoop :: Board -> Mark -> IO ()
gameLoop bd mk = do
        putStrLn ("make a play " ++ (show mk) )
        cd <- getLine
        let cd' = (read cd :: (Int,Int))
            newbd = makeMove (Move cd' mk) bd
        print newbd
        if (isOver newbd) 
                then (putStrLn ((show mk) ++ " is winner")) 
                else (gameLoop newbd (changePlayer mk))
