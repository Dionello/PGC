module HanoiTower where
  data Disk = Disk {weight :: Int} deriving (Show, Eq)
  data Rod = Rod {stack :: [Disk]} deriving (Show, Eq)
  data Pos = L | M | R deriving (Show, Eq)
  data Towers = Towers { left :: Rod
                       , middle :: Rod
                       , right :: Rod
                       } deriving (Show)

  newRod :: Int -> Rod
  newRod n = Rod (map Disk [1..n])

  emptyRod :: Rod
  emptyRod = Rod []

  newTower :: Int -> Towers
  newTower n = Towers (newRod n) emptyRod emptyRod


  takeFrom :: Rod -> (Disk, Rod)
  takeFrom r = (head disks, Rod (tail disks))
            where disks = stack r

  placeAt :: Disk -> Rod -> Rod
  placeAt d r = Rod ((stack r) ++ [d])

  makeMove :: Towers -> Pos -> Pos -> Towers
  makeMove tw f t | (f,t) == (L,M) = Towers (from l) (to l m) (r)
                  | (f,t) == (L,R) = Towers (from l) (m) (to l r)
                  | (f,t) == (M,L) = Towers (to m l) (from m) (r)
                  | (f,t) == (M,R) = Towers (l) (from m) (to m r)
                  | (f,t) == (R,L) = Towers (to r l) (m) (from r)
                  | (f,t) == (R,M) = Towers (l) (to r m) (from r)
                  | otherwise    = tw
                    where
                      l = left tw
                      m = middle tw
                      r = right tw
                      from x = snd (takeFrom x)
                      to x y = placeAt (fst (takeFrom x)) y

  allowedRod :: Rod -> Bool
  allowedRod r = ordered (map weight (stack r))
                 where ordered (l:[]) = True
                       ordered (l:m:ls) | l > m = False
                                        | otherwise = ordered (m:ls)

  isOver :: Towers -> Bool
  isOver tw =   left tw == emptyRod
             && middle tw == emptyRod
             && allowedRod (right tw)
