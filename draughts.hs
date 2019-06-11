-- O jogo é representado por um tabuleiro quadrado de valores Maybe Piece, onde Piece pode assumir
-- os valores Black e White.
data Piece = Black | White deriving (Show, Eq)

type Board = [[Maybe Piece]]

-- o tabuleiro vazio é inicializado com 40% das colunas intercaladas entre Just White e Nothing,
-- 20% das colunas apenas com Nothing, e 40% das colunas intercaladas entre Just Black e Nothing.
emptyBoard :: Int -> Board
emptyBoard n | n < 4     = error "Please choose a number >= 4"
             | otherwise = [[if (odd (x+y)) then Nothing 
             else (Just White) | x <- [1..n]] | y <- [1..qntp]]
             ++ [[ Nothing | x <- [1..n]] | y <- [1..qnte]]
             ++ [[if (odd (x+y)) then Nothing 
             else (Just Black) | x <- [1..n]] | y <- [1..qntp]]
              where qntp = floor ((0.4) * (fromIntegral n))
                  qnte  = n - (2*qntp)

type Coord = (Int,Int)

data Quadrant = Q1 | Q2 | Q3 | Q4
data MoveType = Walk | Jump
type Move = (MoveType, Coord, Quadrant)

-- Calcula a coordenada destinado de um movimento
destination :: Move -> Coord
destination (Walk,cd,Q1) = ((fst cd+1),(snd cd+1))
destination (Walk,cd,Q2) = ((fst cd-1),(snd cd+1))
destination (Walk,cd,Q3) = ((fst cd-1),(snd cd-1))
destination (Walk,cd,Q4) = ((fst cd+1),(snd cd-1))
destination (Jump,cd,qd) 
        = destination (Walk,destination (Walk,cd,qd),qd)
        --Aplica Walk 2 vezes

-- notIn: retorna True caso a coordenada não esteja dentro do tabuleiro, e False caso esteja.
notIn :: Coord -> Board -> Bool
coord `notIn` board 
        | (fst coord >= l) = True
        | (snd coord >= l) = True
        | otherwise        = False
        where l = length board

-- Verifica se o movimento pode ser realizado
isValid :: Move -> Board -> Bool
isValid (mv,cd,qd) bd | cd `notIn` bd = False
                      | dest `notIn`  bd = False
                      | spaceAt (dest) bd /= Nothing  = False
                      | (mv == Jump) && (destination (Walk,cd,qd) == Nothing) = False
                      | otherwise = True
                        where dest = destination (mv,cd,qd)

-- retorna o símbolo de determinada coordenada do tabuleiro
spaceAt :: Coord -> Board -> Maybe Piece
spaceAt (x,y) board = (board !! x) !! y

-- insere um símbolo em determina coordenada no tabuleiro
insertAt :: Maybe Piece -> Coord -> Board -> Board
insertAt p (x,y) bd 
        = take x bd ++ [(take y lineX ++ [p] ++ drop (y+1) lineX)] ++ drop (x+1) bd 
        where lineX = bd !! x

-- executa uma jogada no tabuleiro
makeMove :: Move -> Board -> Board
makeMove (Walk,cd,qd) bd | isValid (Walk,cd,qd)
                   = 
                   where = dt = destination Move

-- verifica se o jogo encerrou
isOver :: Board -> Bool
isOver bd | or (map (elem (Just White)) bd) = False
          | or (map (elem (Just Black)) bd) = False
          | otherwise                       = True
