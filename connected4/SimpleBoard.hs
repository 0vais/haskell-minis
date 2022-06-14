{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleBoard where

import Control.Monad.Reader
import Logic
import Types

data SimpleBoard = SimpleBoard Config [[Player]]

instance Board SimpleBoard where
  emptyBoard cfg@Config {..} = SimpleBoard cfg $ replicate numOfCols []

  spacesLeftInCol cn (SimpleBoard Config {..} board) =
    numOfRows - length (board !! (cn - 1))

  isBoardFull :: SimpleBoard -> Bool
  isBoardFull (SimpleBoard Config {..} board) =
    all (\c -> length c == numOfRows) board

  place :: ColNo -> Player -> SimpleBoard -> SimpleBoard
  place cn player (SimpleBoard cfg@Config {..} board) =
    let col = board !! (cn - 1)
        newBoard = take (cn - 1) board ++ [player : col] ++ drop cn board
     in SimpleBoard cfg newBoard

  playerAt :: Cell -> SimpleBoard -> (Maybe Player)
  playerAt (cn, rn) sb@(SimpleBoard cfg@Config {..} board) =
    let col = board !! (cn - 1)
        rowsFilled = length col
     in if rn > rowsFilled
          then Nothing
          else Just $ col !! (rowsFilled - rn)

  won :: ColNo -> SimpleBoard -> Bool
  won cn sb@(SimpleBoard cfg@Config {..} board) =
    let rowsFilled = length (board !! (cn - 1))
        allLines = (fmap . fmap) (flip playerAt sb) (rowColDiags cfg (cn, rowsFilled))
     in any (== True) $ containsWin winLength <$> allLines
