module SimpleBoard(SimpleBoard) where

import Logic
import Types

data SimpleBoard = SimpleBoard{
  config :: Config,
  cellsFilled :: Int,
  lastColPlayed :: Maybe Int,
  board :: [[Player]]
                              }

instance Board SimpleBoard where
  emptyBoard cfg@Config {..} = SimpleBoard cfg 0 Nothing $ replicate numOfCols []

  colHasSpace cn (SimpleBoard Config{..} _ _ board) =
    numOfRows > length (board !! (cn - 1))

  numOfMoves (SimpleBoard _ cellsFilled _ _) = cellsFilled

  isBoardFull :: SimpleBoard -> Bool
  isBoardFull (SimpleBoard Config {..} cellsFilled _ _) =
    cellsFilled >= numOfCols * numOfRows

  place :: ColNo -> SimpleBoard -> SimpleBoard
  place cn sb@(SimpleBoard cfg@Config {..} cellsFilled _ board) =
    let col = board !! (cn - 1)
        player = turn sb
        newBoard = take (cn - 1) board ++ [player : col] ++ drop cn board
     in SimpleBoard cfg (cellsFilled+1) (Just cn) newBoard

  playerAt :: Cell -> SimpleBoard -> (Maybe Player)
  playerAt (cn, rn) sb@(SimpleBoard cfg@Config {..} _ _ board) =
    let col = board !! (cn - 1)
        rowsFilled = length col
     in if rn > rowsFilled
          then Nothing
          else Just $ col !! (rowsFilled - rn)

  won :: SimpleBoard -> Bool
  won (SimpleBoard _ _ Nothing _) = False
  won sb@(SimpleBoard cfg@Config {..} _ (Just cn) board) =
    let rowsFilled = length (board !! (cn - 1))
        allLines = (fmap . fmap) (flip playerAt sb) (rowColDiags cfg (cn, rowsFilled))
     in any (== True) $ containsWin winLength <$> allLines
