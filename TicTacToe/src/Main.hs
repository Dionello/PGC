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
                      let c = if (ms /= []) then '|' else ' '
                      putChar c
                      printLine ms

printBoard :: Board -> IO ()
printBoard bd = mapM_ printLine bd


main = do
  putStrLn "Tic Tac Toe"
  let board = emptyBoard
  printBoard board
  gameLoop board X


nextPlayer :: Mark -> Mark
nextPlayer X = O
nextPlayer O = X

gameLoop :: Board -> Mark -> IO ()
gameLoop bd mk = do
        putStrLn ("make a play " ++ (show mk) )
        cd <- getLine
        let cd' = (read cd :: (Int,Int))
            newbd = makeMove (Move cd' mk) bd
        printBoard newbd
        if (isOver newbd)
                then (putStrLn ((show mk) ++ " is winner"))
                else (gameLoop newbd (nextPlayer mk))
