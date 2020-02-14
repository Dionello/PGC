module Queens where
  data Square = Queen | Empty deriving(Show, Eq)
  type Board = [[Square]]

  emptyBoard :: Board
  emptyBoard = [[ Empty | x <- [0..2]] | y <- [0..2]]

  type Coord = (Int, Int)

  lineFree :: Board -> Int -> Bool
  lineFree bd x | elem Queen linex = False
                | otherwise        = True
                  where linex = (bd !! x)


  columnFree :: Board -> Int -> Bool
  columnFree bd y | elem Queen columny = False
                  | otherwise          = True
                    where columny = map (!!y) bd

  insertAt :: Board -> Coord -> Board
  insertAt bd (x,y)
          = take x bd ++ [(take y lineX ++ [Queen] ++ drop (y+1) lineX)] ++ drop (x+1) bd
          where lineX = bd !! x

  insertQueen :: Board -> Coord -> Board
  insertQueen bd (x,y) | lineFree bd x && columnFree bd y
                           = insertAt bd (x,y)
                       | otherwise = bd
