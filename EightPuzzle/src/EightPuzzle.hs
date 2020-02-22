module EightPuzzle where
  data Move = Up | Down | Lft | Rgt deriving (Show, Eq)
  type Frame = [[Space]]
  data Space = Tile Int | Empty deriving (Show,Eq)
  type Pos = (Int, Int)

  mapTile :: [Int] -> [Space]
  mapTile [] = []
  mapTile (l:ls) | l == 0    = Empty:(mapTile ls)
                 | otherwise = (Tile l):(mapTile ls)


  newFrame :: [Int] -> Frame
  newFrame a | and [f a | f<-(elem<$>[0..8])] && length a == 9
                = split (mapTile a)
             | otherwise = []
               where split l = (take 3 l) : (drop 3 (take 6 l)) : (drop 6 l) : []

  valueAt :: Pos -> [[a]] -> a
  valueAt (x,y) t = t !! x !! y

  insertAt :: a -> Pos -> [[a]] -> [[a]]
  insertAt a (x,y) t = take x t
                       ++ [(take y lineX ++ [a] ++ drop (y+1) lineX)]
                       ++ drop (x+1) t
                       where lineX = t !! x

  swap :: Pos -> Pos -> [[a]] -> [[a]]
  swap posa posb t = insertAt a posb (insertAt b posa t)
                    where
                      a = valueAt posa t
                      b = valueAt posb t

  makeMove :: Frame -> Pos -> Move -> Frame
  makeMove fr pos mv =
      case mv of
        Up   -> swap pos (x-1,y) fr
        Down -> swap pos (x+1,y) fr
        Lft  -> swap pos (x,y-1) fr
        Rgt  -> swap pos (x,y+1) fr
        where
          x = fst pos
          y = snd pos

  isOver :: Frame -> Bool
  isOver fr = (newFrame [0..8]) == fr
