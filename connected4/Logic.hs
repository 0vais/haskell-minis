module Logic where

import Data.Ix (inRange)
import Data.List (group, unfoldr)
import Types
import Data.Bits ((.&.))

toggle :: Player -> Player
toggle First = Second
toggle Second = First

-- for the given cell, it returns row, col and both diagnals containing this cell
-- since we only need to connect winLength so maximum (2*winLength-1) cells are
-- required to check in each row,col or diagnols.
-- eg For (4,4) cell, in row we only check [(1,4),(2,4)..(7,4)] i.e. 7 cells
-- eg For (4,4) cell, in col we only check [(4,1),(4,2)..(4,7)] i.e. 7 cells
-- eg For (4,4) cell, in diagnal 1 we only check [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1)] i.e. 6 cells.
-- eg For (4,4) cell, in diagnal 3 we only check [(7,1),(6,2),(5,3),(4,4),(3,5),(2,6)] i.e. 6 cells.
-- how to find these cells: simple - take `winLength-1` cells forward 'adding' delta and backward `subtracting` delta
-- where delta is based on row, col or diagnal.
rowColDiags :: Config -> Cell -> [[Cell]]
rowColDiags Config {..} (cno, rno) =
  mkRow (cno, rno) <$> [(-1, 0), (0, -1), (1, 1), (1, -1)]
  where
    mkRow (cn, rn) (dc, dr) = reverse (go (dc, dr) (cn, rn)) ++ tail (go (-dc, -dr) (cn, rn))
    go (dc, dr) = takeWhile inBox . take winLength . iterate (\(cn', rn') -> (cn' + dc, rn' + dr))
    inBox (cn, rn) = inRange (1, numOfCols) cn && inRange (1, numOfRows) rn

containsWin :: Int -> [Maybe Player] -> Bool
containsWin winLength colors =
  if groups == []
    then False
    else maximum groups >= winLength
  where
    groups = fmap length . filter ((/= Nothing) . head) . group $ colors
