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
--verificada se a coordenada passada está dentro do tabuleiro

spaceAt :: Coord -> Board -> Maybe Mark
spaceAt (x,y) board = (board !! x) !! y
-- retorna o símbolo de determinada coordenada do tabuleiro

isValid :: Move -> Board -> Bool
isValid (mk,cd) board
            | cd `notIn` board                = False
            | spaceAt cd board == Just Nought = False
            | spaceAt cd board == Just Cross  = False
            | otherwise                       = True
-- verifica se uma jogada é válida no tabuleiro atual

insertAt :: Mark -> Coord -> Board -> Board
insertAt mk (x,y) bd 
        = take x bd ++ [(take y lineX ++ [Just mk] ++ drop (y+1) lineX)] ++ drop (x+1) bd 
        where lineX = bd !! x
-- insere um símbolo em determina coordenada no tabuleiro

makeMove :: Move -> Board -> Board
makeMove (mk,(x,y)) bd
            | isValid (mk,(x,y)) bd
              = insertAt mk (x,y) bd
            |otherwise  = bd
-- executa uma jogada no tabuleiro

isOver :: Board -> Bool
isOver bd = not (or (map (Nothing `elem`) bd))
-- verifica se o jogo terminou
