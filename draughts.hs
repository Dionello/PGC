-- O jogo é representado por um tabuleiro quadrado de valores Maybe Piece, onde Piece pode assumir
-- os valores Black e White.
data Piece = Black | White deriving (Show)

type Board = [[Maybe Piece]]

-- o tabuleiro vazio é inicializado com todos os valores Nothing
emptyBoard :: Int -> Board
emptyBoard n = [[ Nothing | x <- [1..n]] | y <- [1..n]]

type Coord = (Int,Int)

type Move = (Coord, Coord)

-- isIn: retorna True caso a coordenada esteja dentro do tabuleiro. Caso contrário, retorna False
isIn :: Coord -> Board -> Bool
coord `isIn` board = if (fst coord >l) || (snd coord >l)
                      then False
                      else True
                      where l = length board

isValid :: Move -> Board -> Bool
isValid move board | initial `notIn` Board = False
                   | final   `notIn` Board = False
                   |

                   where initial = (fst move), final = (snd move)



makeMove :: Move -> Board -> Board

