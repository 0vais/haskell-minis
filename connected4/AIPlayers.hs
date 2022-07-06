module AIPlayers(NaiveAIPlayer(..), DecentAIPlayer(..)) where

import GameSearch
import Types
import Heuristics (smartHeuristic, positionalHeuristic, allCollinear)

data NaiveAIPlayer = NaiveAIPlayer

instance Player NaiveAIPlayer where
  nextMove brd _ = return $ bestMove positionalHeuristic nextPossibleBoards maxBound brd 2

data DecentAIPlayer = DecentAIPlayer

instance Player DecentAIPlayer where
  nextMove brd _ = return $ bestMove (smartHeuristic $ allCollinear $ config brd) nextPossibleBoards maxBound brd 6
