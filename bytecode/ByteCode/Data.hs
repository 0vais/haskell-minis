{-# LANGUAGE OverloadedStrings #-}
module ByteCode.Data where

import Data.Text (Text)
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as M
import TextShow (Builder)

type Val = Integer

type Var = Text

type Label = Text

data ByteCode
  = LOAD Val
  | WRITE Var
  | READ Var
  | ADD
  | MULT
  | SUB
  | DIV
  | RETURN
  | LABEL Label
  | CJUMP Label
  deriving (Eq,Show)

data PEnv = PEnv
  { byteCodes :: IntMap ByteCode,
    labels :: Map Label Int
  }

data PState = PState
  { memTable :: Map Var Val,
    stack :: [Val],
    pc :: Int,
    output :: Builder
  }

emptyPState = PState M.empty [] 0 "Empty Program"
