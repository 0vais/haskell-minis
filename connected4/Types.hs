{-# LANGUAGE FlexibleContexts #-}
module Types where
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError)


data Player = First | Second deriving (Ord, Eq)

type ColNo = Int

type RowNo = Int

type Cell = (ColNo, RowNo)

data Config = Config
  { numOfRows :: Int,
    numOfCols :: Int,
    winLength :: Int
  }

data GameState brd = GameState
  { board :: brd,
    turn :: Player
  }

data Error
  = INVALID_COL ColNo
  | FULL_COL ColNo
  | INTERNAL_ERROR

type Game brd = ReaderT Config (StateT (GameState brd) (ExceptT Error IO))

class Board a where
  emptyBoard :: Config -> a
  spacesLeftInCol :: ColNo -> a -> Int
  isBoardFull :: a -> Bool
  place :: ColNo -> Player -> a -> a
  playerAt :: Cell -> a -> Maybe Player

  -- perhaps this should not be here
  won :: ColNo -> a -> Bool
