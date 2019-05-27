data Piece = Black | White deriving (Show)

type Board = [[Maybe Piece]]

--emptyBoard :: Int -> Board
--emptyBoard n = [[ Nothing | x <- 

type Coord = (Int,Int)

type Move = (Coord, Coord)

notIn :: Coord -> Board -> Bool
coord `notIn` Board = if (fst coord >l) || (snd coord >l)
                      then False
                      else True
                      where l = length Board

isValid :: Move -> Board -> Bool
isValid move board | initial `notIn` Board = False
                   | final   `notIn` Board = False
                   |

                   where initial = (fst move), final = (snd move)



makeMove :: Move -> Board -> Board

