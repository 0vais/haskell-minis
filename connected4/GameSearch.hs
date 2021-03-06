{-# LANGUAGE ScopedTypeVariables #-}

module GameSearch where

import Data.Maybe (fromJust)
import Debug.Trace
import Types (Board (pastMoves))

bestMove ::
  (Num score, Ord score) =>
  (board -> score) ->
  (board -> [board]) ->
  score ->
  board ->
  Int ->
  board
bestMove heuristic getNextMoves bound board depth =
  let negamax board depth alpha beta
        | depth <= 0 = (Nothing, heuristic board)
        | otherwise = case getNextMoves board of
            [] -> (Nothing, heuristic board)
            nextMoves -> loop nextMoves Nothing alpha
              where
                loop [] bestMove alpha = (bestMove, alpha)
                loop (brd : brds) bestMove alpha =
                  let (_, score) = negamax brd (depth - 1) (-beta) (-alpha)
                      score' = -score
                      (bestMove', alpha') =
                        if score' > alpha then (Just brd, score') else (bestMove, alpha)
                   in if alpha' >= beta
                        then (bestMove', alpha')
                        else loop brds bestMove' alpha'
      rs = negamax board depth (-bound) bound
   in fromJust . fst $ rs
