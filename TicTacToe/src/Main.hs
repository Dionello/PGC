module Main where

import Game
--implementação estética do printBoard
marktoChar :: Maybe Mark -> Char
marktoChar (Just X)  = 'X'
marktoChar (Just O)  = 'O'
marktoChar (Nothing) = ' '

printLine :: [Maybe Mark] -> IO ()
printLine [] = putChar '\n'
printLine (m:ms) = do putChar (marktoChar m)
                      putChar (if (ms == []) then ' '  else '|')
                      printLine ms

printBoard :: Board -> IO ()
printBoard bd = mapM_ printLine bd

main = do
  putStrLn "Tic Tac Toe"
  putStrLn "Choose the board size"
  nl <- getLine
  let n = (read nl :: Int) 
      board = emptyBoard n
  print board
  gameLoop board Cross
  
nextPlayer :: Mark -> Mark
nextPlayer X = O
nextPlayer O = X

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
