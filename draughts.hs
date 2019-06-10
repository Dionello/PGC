-- O jogo é representado por um tabuleiro quadrado de valores Maybe Piece, onde Piece pode assumir
-- os valores Black e White.
data Piece = Black | White deriving (Show)

type Board = [[Maybe Piece]]

-- o tabuleiro vazio é inicializado com todos os valores Nothing
emptyBoard :: Int -> Board
emptyBoard n = [[ Nothing | x <- [1..n]] | y <- [1..n]]

type Coord = (Int,Int)

data Quadrant = Q1 | Q2 | Q3 | Q4
data MoveType = Walk | Jump
type Move = (MoveType, Coord, Quadrant)

destination :: Move -> Coord
destination (Walk,cd,Q1) = ((fst cd+1),(snd cd+1))
destination (Walk,cd,Q2) = ((fst cd-1),(snd cd+1))
destination (Walk,cd,Q3) = ((fst cd-1),(snd cd-1))
destination (Walk,cd,Q4) = ((fst cd+1),(snd cd-1))
destination (Jump,cd,qd) 
        = destination (Walk,destination (Walk,cd,qd),qd)
        --Aplica Walk 2 vezes

-- notIn: retorna True caso a coordenada esteja dentro do tabuleiro, e False caso esteja.
notIn :: Coord -> Board -> Bool
coord `notIn` board 
        | (fst coord >= l) = True
        | (snd coord >= l) = True
        | otherwise        = False
        where l = length board

-- Verifica se o movimento pode ser realizado
isValid :: Move -> Board -> Bool
isValid (mv,cd,qd) board | cd `notIn` Board = False
                         | cd `notIn` Board = False
                         | destination (mv,cd,qd) `notIn` Board = False
                         | otherwise = True


makeMove :: Move -> Board -> Board

isOver :: Board -> Bool
isOver bd | or (map (elem (Just White)) bd) = False
          | or (map (elem (Just Black)) bd) = False
          | otherwise                       = True
