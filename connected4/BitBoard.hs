{-# LANGUAGE OverloadedRecordDot #-}
module BitBoard(BitBoard) where

import Data.Bit
import Data.Bits ((.&.), Bits (xor, (.|.), shiftR))
import Data.Maybe
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Types
import Logic

type BitVector = U.Vector Bit

data BitBoard = BitBoard
  { config :: Config,
    totalCells :: Int,
    cellsFilled :: Int,
    board :: BitVector,
    mask :: BitVector,
    topMasks :: B.Vector BitVector,
    botMasks :: B.Vector BitVector
  }

isAnyBitSet = isJust . bitIndex 1

instance Board BitBoard where
  emptyBoard cfg@Config {..} = BitBoard cfg total 0 empty empty topMasks botMasks
    where
      total = numOfCols * numOfRows
      empty = U.replicate total 0
      topMasks = B.fromList [empty U.// [(cn * numOfRows - 1, 1)] | cn <- [1 .. numOfCols]]
      botMasks = B.fromList [empty U.// [(cIndex * numOfRows, 1)] | cIndex <- [0 .. (numOfCols-1)]]

  colHasSpace cn BitBoard {..} = not . isAnyBitSet $ mask .&. (topMasks B.! (cn-1))

  numOfMoves = cellsFilled

  isBoardFull BitBoard {..} = cellsFilled >= totalCells

  place cn bb@BitBoard {..} = bb{cellsFilled = (cellsFilled+1), board=board', mask=mask'} where
    board' = board `xor` mask
    offset = (cn-1) * config.numOfRows
    zeroIndex = bitIndex 0 . U.take config.numOfRows . U.drop offset $ mask
    mask' = case zeroIndex of
      Just idx -> mask U.// [(offset + idx, 1)]
      Nothing -> mask

  playerAt (cn,rn) bb@BitBoard {..} = if isFilled then Just player else Nothing where
    bitIndex = (cn-1) * config.numOfRows + (rn-1)
    (Bit isFilled) = mask U.! bitIndex
    (Bit isCurrentPlayer) = board U.! bitIndex
    currentPlayer = turn bb
    player = if isCurrentPlayer then currentPlayer else toggle currentPlayer

  won BitBoard {..} = rs where
    board' = board `xor` mask
    mkShifts d = foldr1 (.&.) $ shiftR board' <$> [0,d..(config.winLength - 1) * d]
    --delta for [vert,hor,d1,d2]
    deltaForEachDirection = [1,config.numOfRows, config.numOfRows + 1, config.numOfRows - 1 ]
    rs = any isAnyBitSet $ fmap mkShifts deltaForEachDirection
