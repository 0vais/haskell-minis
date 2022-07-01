-- | This module contains different scoring heuristics
module Heuristics where

import Types
import Data.Ix
import Logic (toggle)
import Data.Monoid

-- It assumes that closer a player plays to the middle better are the chances to win.
positionalHeuristic :: (Board board) => board -> Int
positionalHeuristic board =
  let cfg = config board
      midr = numOfRows cfg `div` 2
      midc = numOfCols cfg `div` 2
      maxScore = numOfRows cfg + numOfCols cfg
      cn = head $ pastMoves board
      rn = (numOfRows cfg) - numSpacesInCol cn board
   in if isBoardFull board
        then 0
        else
          if won board
            then -(maxBound-1)
            else (abs (midr - rn) + abs (midc - cn)) - maxScore

smartHeuristic :: (Board board) => [[Cell]] -> board -> Int
smartHeuristic allCollinearCells board =
  if won board
    then (-(maxBound-1))
    else
      if isBoardFull board
        then 0
        else getSum $ foldMap (Sum . score) allCollinearCells
  where
    currentPlayer = turn board
    noc = numOfCols $ config board
    nor = numOfRows $ config board
    wl = (winLength $ config board)
    filledCellScore = (max noc nor)^2
    emptyCellScore = nor
    cellScore cell@(cn,rn) = let mbPlayer = playerAt cell board
                                 spacesLeft = numSpacesInCol cn board
                                 filledCount = nor - spacesLeft
                                 emptyBelowRN = rn - filledCount - 1
                                 emptyCellScore' = emptyCellScore - emptyBelowRN
                                 in case mbPlayer of
                                      Nothing -> (emptyCellScore', emptyCellScore')
                                      (Just player) -> if (player == currentPlayer) then (filledCellScore, 0)
                                                        else (0, filledCellScore)
    score :: [Cell] -> Int
    score cells = let wcells = take wl cells
                      (currentPlyrScr,oppPlyrScr) = foldr prod (1,1) wcells
                      prod cell (prod1, prod2) = let (scr1, scr2) = cellScore cell
                                                        in (scr1 * prod1, scr2 * prod2)
                      in if length wcells < wl then 0
                         else score (tail cells) + (currentPlyrScr - oppPlyrScr)

allCollinear :: Config -> [[Cell]]
allCollinear Config{..} = concat [allHz, allVert, allD1, allD2] where
  collinear cell (dc,dr) = takeWhile inBox . iterate (\(cn', rn') -> (cn' + dc, rn' + dr)) $ cell
  inBox (cn, rn) = inRange (1, numOfCols) cn && inRange (1, numOfRows) rn
  stHz = collinear (1,1) (0,1)
  allHz = flip collinear (1,0) <$> stHz
  stVert = collinear (1,1) (1,0)
  allVert = flip collinear (0,1) <$> stVert
  stD1 = collinear (1,winLength) (0,1) ++ collinear (2,numOfRows) (1,0)
  allD1 = flip collinear (1,-1) <$> stD1
  stD2 = collinear (winLength,1) (-1,0) ++ collinear (1,2) (0,1)
  allD2 = flip collinear (1,1) <$> stD2
