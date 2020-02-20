module EightPuzzle where
  import System.Random
  data Space = Tile Int | Empty deriving (Show,Eq)
  type Frame = [[Space]] deriving (Show)

  validFrame :: Frame -> Bool
  validFrame fr = 1 == length $ filter (==Empty) $ join fr

  copy n = (take n).repeat

  makeFrame :: Int -> Frame
  makeFrame n = copy n $ copy n Empty
  newFrame :: Int -> Frame
  newFrame n = newFrame' n & makeFrame n
            where
              newFrame' n fr | validFrame fr = fr
                             | otherwise = newFrame
