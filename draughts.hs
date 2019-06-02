-- O jogo é representado por um tabuleiro quadrado de valores Maybe Piece, onde Piece pode assumir
-- os valores Black e White.
data Piece = Black | White deriving (Show)

type Board = [[Maybe Piece]]

-- o tabuleiro vazio é inicializado com todos os valores Nothing
emptyBoard :: Int -> Board
emptyBoard n = [[ Nothing | x <- [1..n]] | y <- [1..n]]

type Coord = (Int,Int)

type Move = (Coord, Coord)

-- notIn: retorna True caso a coordenada esteja dentro do tabuleiro, e False caso esteja.
notIn :: Coord -> Board -> Bool
coord `notIn` board 
        | (fst coord >= l) = True
        | (snd coord >= l) = True
        | otherwise        = True
        where l = length board
        
-- isValid verifica se uma jogada é valida
--
--isValid :: Move -> Board -> Bool
--isValid move board | (fst move) `notIn` Board = False
--                   | (snd move) `notIn` Board = False
--                   |

isValid :: Move -> Board -> Bool
isValid move board | (fst move) `notIn` Board = False
                   | (snd move) `notIn` Board = False
                   |


makeMove :: Move -> Board -> Board

