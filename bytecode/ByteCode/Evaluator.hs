{-
(1)

    You are a TA at a university, and you want to evaluate your studentâ€™s homework
    without executing their (untrusted) code. You decide to write a small
    web-service that takes bytecode as input, and interprets the results.

    The bytecode language you need to support includes basic arithmetic and
    variables. The bytecode language is stack, rather than register based.
    ByteCode (right) is given for the following pseudo code (left):

    function f() {

        x = 1                   LOAD_VAL 1
                                WRITE_VAR â€˜xâ€™

        y = 2                   LOAD_VAL 2
                                WRITE_VAR â€˜yâ€™

        return (x + 1) * y      READ_VAR â€˜xâ€™
                                LOAD_VAL 1
                                ADD

                                READ_VAR â€˜yâ€™
                                MULTIPLY

                                RETURN_VALUE
    }

Add a data type `ByteCode` that can represent bytecode like in the example
above, along with an interpreter for said bytecode. Make sure your code is
total.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import TextShow

import ByteCode.Data
import ByteCode.Parser
import qualified Data.Text.IO as TIO
import qualified TextShow as TIO

data PError
  = VAR_NOT_DEFINED Var
  | LABEL_NOT_DEFINED Label
  | STACK_UNDERFLOW
  | DIV_BY_ZERO
  | INTERNAL_ERROR

instance TextShow PError where
  showb (VAR_NOT_DEFINED v) = "Variable <<" <> fromText v <> ">> is not defined"
  showb (LABEL_NOT_DEFINED l) = "Label <<" <> fromText l <> ">> is not defined"
  showb DIV_BY_ZERO = "Division by zero"
  showb STACK_UNDERFLOW = "Stack under flow"
  showb INTERNAL_ERROR = "Internal error! Program is in incorrect state."

type EvalM = ReaderT PEnv (StateT PState (ExceptT PError Identity))

mkPEnv :: [ByteCode] -> PEnv
mkPEnv byteList = PEnv{byteCodes = byteCodes, labels = labels} where
  zipped = zip [1..] byteList
  labels = M.fromList $
    (\case (i, LABEL lbl) -> (lbl,i); _ -> error "Impossible?")
    <$> filter (\case (_,LABEL _) -> True; _ -> False) zipped
  byteCodes = IM.fromList zipped

push :: Val -> EvalM ()
push i = modify (\case ps@PState {..} -> ps {stack = i : stack})

pop :: EvalM Val
pop = do
  PState {..} <- get
  case stack of
    [] -> throwError STACK_UNDERFLOW
    (x : xs) -> modify (\pst -> pst {stack = xs}) >> return x

arithmetic :: (Val -> Val -> Val) -> EvalM ()
arithmetic op = pop >>= \v1 -> pop >>= \v2 -> push (v2 `op` v1)

saveOutput :: Builder -> EvalM ()
saveOutput out = modify (\pst -> pst {output = out})

evalByteCode :: ByteCode -> EvalM ()
evalByteCode byteCode = do
  PEnv {..} <- ask
  PState {..} <- get
  case byteCode of
    LOAD val -> push val >> saveOutput "Load"
    READ var ->
      maybe
        (throwError $ VAR_NOT_DEFINED var)
        (\val -> push val >> saveOutput "Read")
        (memTable M.!? var)
    WRITE var ->
      pop >>= \val ->
        modify (\st -> st {memTable = M.insert var val memTable})
          >> saveOutput "Write"
    ADD -> arithmetic (+) >> saveOutput "Add"
    MULT -> arithmetic (*) >> saveOutput "Mult"
    SUB -> arithmetic (-) >> saveOutput "Sub"
    DIV ->
      pop >>= \v1 ->
        pop >>= \v2 ->
          if v1 /= 0
            then push (v2 `div` v1) >> saveOutput "Div"
            else throwError DIV_BY_ZERO
    LABEL _ -> return ()
    CJUMP lbl ->
      pop >>= \val ->
        if val /= 0
          then
            maybe
              (throwError $ LABEL_NOT_DEFINED lbl)
              (\v -> modify (\st -> st {pc = v}) >> saveOutput "CJUMP done")
              (labels M.!? lbl)
          else saveOutput "CJUMP not done"
    RETURN -> pop >>= \v -> saveOutput $ "Return: " <> showb v

interpreter :: EvalM Builder
interpreter = do
  modify (\st@PState{..} -> st {pc = pc + 1})
  PEnv {..} <- ask
  PState {..} <- get
  if pc > IM.size byteCodes
    then return output
    else case byteCodes IM.!? pc of
      Just byteCode -> evalByteCode byteCode >> interpreter
      Nothing -> throwError INTERNAL_ERROR

runByteCode :: Text -> Text
runByteCode txt = toText rs
  where
    penv = runIdentity . runExceptT $ mkPEnv <$> parser txt
    interprete = fmap fst . runIdentity . runExceptT . flip runStateT emptyPState . runReaderT interpreter
    rss = interprete <$> penv
    rs = case rss of
      Left serr -> "Syntax Error: " <> showb serr
      Right (Left perr) -> "Error: " <> showb perr
      Right (Right out) -> "Success: " <> out

main :: IO ()
main = TIO.interact runByteCode

exampleMain :: IO ()
exampleMain = TIO.printT $ runByteCode input1

fib10 :: IO ()
fib10 = TIO.printT $ runByteCode (fibn 10)

serror0 :: Text
serror0 = "LOAD_VAL4"

serror1 :: Text
serror1 = "LOAD_VAL 4  \nADD"

serror2 :: Text
serror2 = "LOAD_VAL 4\nADD ab"

perror0 :: Text
perror0 = "RETURN_VALUE"

perror1 :: Text
perror1 = "READ_VAR â€˜xâ€™"

perror2 :: Text
perror2 = "LOAD_VAL 4\nADD"

perror3 :: Text
perror3 = "LOAD_VAL 5\nCJUMP MMM"

perror4 :: Text
perror4 = "LOAD_VAL 1\nLOAD_VAL 0\nDIVIDE\nRETURN_VALUE"

input0 :: Text
input0 = "LOAD_VAL 1"

input1 :: Text
input1 = "LOAD_VAL 1\n\
         \WRITE_VAR â€˜xâ€™\n\
         \LOAD_VAL 2\n\
         \WRITE_VAR â€˜yâ€™\n\
         \READ_VAR â€˜xâ€™\n\
         \LOAD_VAL 1\n\
         \ADD\n\
         \READ_VAR â€˜yâ€™\n\
         \MULTIPLY\n\
         \RETURN_VALUE"

fibn :: Int -> Text
fibn n | n < 0 = "RETURN_VALUE"
fibn n | n == 0 = "LOAD_VAL 0\nRETURN_VALUE"
fibn n | n == 1 = "LOAD_VAL 1\nRETURN_VALUE"
fibn n = "LOAD_VAL " <> showt (n-1) <> "\n\
         \WRITE_VAR â€˜countâ€™\n\
         \LOAD_VAL 0\n\
         \WRITE_VAR â€˜aâ€™\n\
         \LOAD_VAL 1\n\
         \LOOP:\n\
         \WRITE_VAR â€˜bâ€™\n\
         \READ_VAR â€˜aâ€™\n\
         \READ_VAR â€˜bâ€™\n\
         \ADD\n\
         \READ_VAR â€˜bâ€™\n\
         \WRITE_VAR â€˜aâ€™\n\
         \READ_VAR â€˜countâ€™\n\
         \LOAD_VAL 1\n\
         \SUBTRACT\n\
         \WRITE_VAR â€˜countâ€™\n\
         \READ_VAR â€˜countâ€™\n\
         \CJUMP LOOP\n\
         \RETURN_VALUE\n\
         \"
