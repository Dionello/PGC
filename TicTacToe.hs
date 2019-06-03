data Mark  = Nought | Cross deriving (Show, Eq)
type Board = [[Maybe Mark]]

emptyBoard :: Int -> Board
emptyBoard n = [[ Nothing | x <- [1..n]] | y <- [1..n]]
-- cria um tabuleiro vazio de tamanho n x n

type Coord = (Int, Int)
type Move = (Mark, Coord)

notIn :: Coord -> Board -> Bool
coord `notIn` board 
        | (fst coord >= l) = True
        | (snd coord >= l) = True
        | otherwise        = False
        where l = length board

spaceAt :: Coord -> Board -> Maybe Mark
spaceAt (x,y) board = (board !! x) !! y

isValid :: Move -> Board -> Bool
isValid (mk,cd) board
            | cd `notIn` board            = False
            | spaceAt cd board == Nothing = True
            | otherwise                   = False
-- verifica se uma jogada é válida no tabuleiro atual

makeMove :: Move -> Board -> Board
makeMove (mk,(x,y)) bd
            | isValid (mk,(x,y)) bd
              = take x bd ++ (take y (bd!!x) ++) ++ drop (x+1) a
-- executa uma jogada no tabuleiro
--isOver :: Board -> Bool
-- verifica se o jogo terminou
