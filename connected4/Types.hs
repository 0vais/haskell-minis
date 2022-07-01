{-# LANGUAGE OverloadedRecordDot #-}

module Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Bits ((.&.))

data Turn = First | Second deriving (Ord, Eq)

type ColNo = Int

type RowNo = Int

type Cell = (ColNo, RowNo)

data Config = Config
  { numOfRows :: Int,
    numOfCols :: Int,
    winLength :: Int
  }

data Error
  = INVALID_COL ColNo
  | FULL_COL ColNo
  | INTERNAL_ERROR

type Game brd = ReaderT Config (StateT brd (ExceptT Error IO))

class Board a where
  emptyBoard :: Config -> a
  config :: a -> Config
  pastMoves :: a -> [ColNo]
  place :: ColNo -> a -> a
  playerAt :: Cell -> a -> Maybe Turn

  -- this method checks if the player who played last turn won
  won :: a -> Bool

  numOfMoves :: a -> Int
  numOfMoves = length . pastMoves

  nextPossibleBoards :: a -> [a]
  nextPossibleBoards brd =
    if isBoardFull brd || won brd
      then []
      else [place cn brd | cn <- [1 .. (numOfCols . config $ brd)], colHasSpace cn brd]

  turn :: a -> Turn
  turn a = if numOfMoves a .&. 1 == 0 then First else Second

  colHasSpace :: ColNo -> a -> Bool
  colHasSpace cn = (> 0) . numSpacesInCol cn

  numSpacesInCol :: ColNo -> a -> Int
  numSpacesInCol cn board = (nor -) . length . filter (== cn) . pastMoves $ board
    where
      nor = numOfRows . config $ board

  isBoardFull :: a -> Bool
  isBoardFull board =
    let cfg = config board
        total = cfg.numOfCols * cfg.numOfRows
     in total == numOfMoves board

class Player p where
  nextMove :: (Board b) => b -> p -> IO b
