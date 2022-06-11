module Types where
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)

data Player = First | Second deriving (Ord, Eq)

type Board = [[Player]]

type ColNo = Int

type RowNo = Int

type Cell = (ColNo, RowNo)

data Config = Config
  { numOfRows :: Int,
    numOfCols :: Int,
    winLength :: Int
  }

data GameState = GameState
  { board :: Board,
    turn :: Player
  }

data Error
  = INVALID_COL ColNo
  | FULL_COL ColNo
  | INTERNAL_ERROR

type Game = ReaderT Config (StateT GameState (ExceptT Error IO))
