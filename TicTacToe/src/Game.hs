module Game where
  import Data.List
-- tenho que capturar uma falha caso o movimento não for correto
-- talvez tenho que deixar a implementação do makeMove só para movimento,
-- e deixar o Main verificar se é valido ou não.
-- Lembre: Um método deve fazer uma ação por vez.


-- Na arvore, eu tenho que verificar se o movimento é possível de ser feito,
-- e fazê-lo. Porém, na função de makeMove, ele confere novamente. Como posso
-- resolver isso? Faço a função makeMove deixar de conferir, e só fazer o
-- movimento??

  data Mark = O | X deriving (Show,Eq)

  size = 3 :: Int -- Tamanho do tabuleiro

  type Board = [[Maybe Mark]]

  -- cria um tabuleiro vazio de tamanho n x n
  emptyBoard :: Board
  emptyBoard = replicate size $
               replicate size Nothing

  type Coord = (Int,Int)

  data Move = Move Coord Mark deriving (Show)

  inRange :: Coord -> Bool
  inRange (x,y) | x < size && y < size = True
                | otherwise            = False

  markAt :: Coord -> Board -> Maybe Mark
  markAt (x,y) bd =  (bd !! x) !! y

  -- verifica se uma jogada é válida no tabuleiro atual
  isValid :: Move -> Board -> Bool
  isValid (Move cd mk) bd | not (inRange cd)        = False
                          | markAt cd bd /= Nothing = False
                          | otherwise               = True


  insertAt :: Mark -> Coord -> Board -> Board
  insertAt mk (x,y) bd = take x bd ++
                        [(take y lineX ++ [pure mk] ++ drop (y+1) lineX)]
                       ++ drop (x+1) bd
                       where lineX = bd !! x

  -- executa uma jogada no tabuleiro
  makeMove :: Move -> Board -> Board
  makeMove (Move cd mk) bd | isValid (Move cd mk) bd
                              = insertAt mk cd bd
                           | otherwise = bd

  columns :: Board -> Board
  columns = transpose

  diagonals :: Board -> Board
  diagonals mt = [[markAt (i,i) mt | i<-[0..size-1] ], -- diagonal descendente
                 [markAt (i,size-i-1) mt | i<-[size-1,size-2..0] ] ] -- diagonal ascendente

  rows :: Board -> [[Maybe Mark]]
  rows bd = bd ++ (columns bd) ++ (diagonals bd)

  -- verifica se o jogo terminou se alguém ganhou ou não há mais jogadas disponíveis
  isOver :: Board -> Bool
  isOver bd = winner bd /= Nothing -- alguém ganhou
           || not (or (map (Nothing `elem`) bd)) -- sem jogadas disponíveis

  -- verifica se o X ganhou
  winner :: Board -> Maybe Mark
  winner bd | crossWin  `elem` rows' = Just X
            | noughtWin `elem` rows' = Just O
            | otherwise              = Nothing
              where rows'     = rows bd
                    crossWin  = replicate size $ Just X
                    noughtWin = replicate size $ Just O
