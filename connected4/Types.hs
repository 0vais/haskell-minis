module Types where
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError)
import Data.Bits ((.&.))


data Player = First | Second deriving (Ord, Eq)

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
  colHasSpace :: ColNo -> a -> Bool
  numOfMoves :: a -> Int
  turn :: a -> Player
  turn a = if numOfMoves a .&. 1 == 0 then First else Second
  isBoardFull :: a -> Bool
  place :: ColNo -> a -> a
  playerAt :: Cell -> a -> Maybe Player

  -- perhaps this should not be here
  -- this method checks if the player who played last turn won
  won :: a -> Bool
