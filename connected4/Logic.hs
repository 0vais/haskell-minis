{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Logic where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Control.Monad.State (StateT, get, put)
import Data.Ix (inRange)
import Data.List (group, unfoldr)
import Types

toggle :: Player -> Player
toggle First = Second
toggle Second = First

place :: ColNo -> Game ()
place cn = do
  Config {..} <- ask
  when (cn < 1 || cn > numOfCols) $ throwError (INVALID_COL cn)
  gameState@GameState {..} <- get
  let col = board !! (cn - 1)
  when (length col >= numOfRows) $ throwError (FULL_COL cn)
  let newBoard = take (cn - 1) board ++ [turn : col] ++ drop cn board
  put gameState {board = newBoard}

isBoardFull :: Game Bool
isBoardFull = do
  GameState {..} <- get
  Config {..} <- ask
  return $ all (\c -> length c == numOfRows) board

won :: ColNo -> Game Bool
won cn = do
  GameState {..} <- get
  cfg@Config {..} <- ask
  let rowsFilled = length (board !! (cn - 1))
      allLines = (fmap . fmap) (playerAt board) (rowColDiags cfg (cn, rowsFilled))
  return $ any (== True) $ containsWin winLength <$> allLines

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

playerAt :: Board -> Cell -> Maybe Player
playerAt board (cn, rn) =
  let col = board !! (cn - 1)
      rowsFilled = length col
   in if rn > rowsFilled
        then Nothing
        else Just $ col !! (rowsFilled - rn)
