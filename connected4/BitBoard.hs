{-# LANGUAGE OverloadedRecordDot #-}
module BitBoard(BitBoard) where

import Data.Bit
import Data.Bits ((.&.), Bits (xor, (.|.), shiftR))
import Data.Maybe
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Types
import Logic
import Debug.Trace

type BitVector = U.Vector Bit

data BitBoard = BitBoard
  { config :: Config,
    cellsFilled :: Int,
    pastMoves :: [ColNo],
    board :: BitVector,
    mask :: BitVector
  }

isAnyBitSet = isJust . bitIndex 1

instance Board BitBoard where
  emptyBoard cfg@Config {..} = BitBoard cfg 0 [] empty empty
    where
      empty = U.replicate (numOfCols * (numOfRows+1)) 0

  config brd = brd.config

  numSpacesInCol cn BitBoard {..} =
    let
      offset = (cn-1) * (config.numOfRows+1)
      zeroIndex = bitIndex 0 . U.take config.numOfRows . U.drop offset $ mask
      in
      case zeroIndex of
        Nothing -> 0
        (Just idx) -> config.numOfRows - idx

  numOfMoves = cellsFilled

  pastMoves brd = brd.pastMoves

  place cn bb@BitBoard {..} = bb{cellsFilled = (cellsFilled+1), board=board', mask=mask', pastMoves = cn:pastMoves} where
    board' = board `xor` mask
    offset = (cn-1) * (config.numOfRows+1)
    zeroIndex = bitIndex 0 . U.take config.numOfRows . U.drop offset $ mask
    mask' = case zeroIndex of
      Just idx -> mask U.// [(offset + idx, 1)]
      Nothing -> mask

  playerAt (cn,rn) bb@BitBoard {..} = if isFilled then Just player else Nothing where
    bitIndex = (cn-1) * (config.numOfRows+1) + (rn-1)
    (Bit isFilled) = mask U.! bitIndex
    (Bit isCurrentPlayer) = board U.! bitIndex
    currentPlayer = turn bb
    player = if isCurrentPlayer then currentPlayer else toggle currentPlayer

  won BitBoard {..} = rs where
    board' = board `xor` mask
    mkShifts d = foldr1 (.&.) $ shiftR board' <$> [0,d..(config.winLength - 1) * d]
    --delta for [vert,hor,d1,d2]
    deltaForEachDirection = [1, config.numOfRows + 1, config.numOfRows, config.numOfRows + 2 ]
    rs = any isAnyBitSet $ fmap mkShifts deltaForEachDirection
