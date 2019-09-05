data Mark = Nought | Cross deriving (Show,Eq)

type Board = [[Maybe Mark]]

-- cria um tabuleiro vazio de tamanho n x n
emptyBoard :: Int -> Board
emptyBoard n = [ [ Nothing | i<-[0..(n-1)] ]
                 | j<-[0..(n-1)] ]   

type Coord = (Int,Int)

data Move = Move Coord Mark

inRange :: Coord -> Board -> Bool
inRange (x,y) bd | x < l && y < l = True
                 | otherwise        = False
                 where l = length bd

markAt :: Coord -> Board -> Maybe Mark
markAt (x,y) bd =  (bd !! x) !! y

-- verifica se uma jogada é válida no tabuleiro atual
isValid :: Move -> Board -> Bool
isValid (Move cd mk) bd | not (inRange cd bd)       = False
                        | (markAt cd bd /= Nothing) = False
                        | otherwise                 = True

-- executa uma jogada no tabuleiro
--makeMove :: Move -> Board -> Board
--makeMove (Move cd mk) bd | isValid (Move cd mk) bd
--                           = 

-- verifica se o jogo terminou
--isOver :: Board -> Bool
