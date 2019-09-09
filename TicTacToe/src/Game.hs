module Game where

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
                 | otherwise      = False
                 where l = length bd

markAt :: Coord -> [[a]] -> a
markAt (x,y) bd =  (bd !! x) !! y

-- verifica se uma jogada é válida no tabuleiro atual
isValid :: Move -> Board -> Bool
isValid (Move cd mk) bd | not (inRange cd bd)       = False
                        | (markAt cd bd /= Nothing) = False
                        | otherwise                 = True


insertAt :: a -> Coord -> [[a]] -> [[a]]
insertAt a (x,y) mt = take x mt 
                     ++ [(take y lineX ++ [a] ++ drop (y+1) lineX)] 
                     ++ drop (x+1) mt
                     where lineX = mt !! x

-- executa uma jogada no tabuleiro
makeMove :: Move -> Board -> Board
makeMove (Move cd mk) bd | isValid (Move cd mk) bd
                            = insertAt (pure mk) cd bd
                         | otherwise = bd

columns :: [[a]] -> [[a]]
columns mt = [map (!!x) mt | x<-[0..(length mt -1)] ]

diagonal :: [[a]] -> [[a]]
diagonal mt = [ [markAt (i,i) mt | i<-[0..l-1] ],
              [markAt (i,l-i-1) mt | i<-[l-1,l-2..0] ] ]
            where l = length mt

-- verifica se o jogo terminou
isOver :: Board -> Bool
isOver bd = crossWin `elem` bd || 
            crossWin `elem` (columns bd) ||
            crossWin `elem` (diagonal bd) ||
            noughtWin `elem` bd ||
            noughtWin `elem` (columns bd) ||
            noughtWin `elem` (diagonal bd) ||
            not (or (map (Nothing `elem`) bd))
            where l = length bd
                  crossWin  = [Just Cross  | i<-[0..l-1]]
                  noughtWin = [Just Nought | i<-[0..l-1]]
