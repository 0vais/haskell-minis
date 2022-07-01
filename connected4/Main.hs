module Main where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import TextShow

import Logic
import Types
import SimpleBoard
import BitBoard(BitBoard)
import SimpleBoard (SimpleBoard)
import AIPlayers

instance TextShow Turn where
  showb First = "One(1)"
  showb Second = "Two(2)"

showBoard :: (Board brd) => Game brd Text
showBoard = do
  Config {..} <- ask
  board <- get
  let showChar cn rn = case playerAt (cn, rn) board of
        Nothing -> "."
        Just First -> "X"
        Just Second -> "0"
      list = [[showChar cn rn | cn <- [1 .. numOfCols]] | rn <- [numOfRows, numOfRows - 1 .. 1]]
  return . T.unlines $ fmap mconcat list

printTextLn = T.putStrLn . toText

prompt :: Turn -> IO Int
prompt player = do
  printTextLn $ "Player " <> showb player <> " Enter the column number [1-7]:"
  xs <- getLine
  if xs /= [] && all isDigit xs then
      return (read xs)
    else do
      T.putStrLn $ "ERROR: Enter only numbers!"
      prompt player

play :: (Board brd) => Game brd ()
play = flip catchError errorHandler $ do
  board <- get
  if isBoardFull board
  then liftIO $ T.putStrLn "It's a draw!!"
  else do
    Config{..} <- ask
    let currentTurn = turn board
    if currentTurn == First then do
      colNo <- liftIO $ prompt $ currentTurn
      when (colNo < 1 || colNo > numOfCols) $ throwError (INVALID_COL colNo)
      when (not $ colHasSpace colNo board) $ throwError (FULL_COL colNo)
      let newBoard = place colNo board
      put newBoard
    else do
      board' <- liftIO $ nextMove board DecentAIPlayer
      put board'
    liftIO $ T.putStrLn "Current Board:\n\n"
    showBoard >>= liftIO . T.putStrLn
    newBoard <- get
    if won newBoard
    then liftIO $ printTextLn $ "Player " <> showb currentTurn <> " won!"
    else play
  where
    errorHandler err = liftIO (errorHandler' err) >> play
    errorHandler' (FULL_COL cn) =
      printTextLn $ "Error! Column: " <> showb cn <> " is full!."
    errorHandler' (INVALID_COL cn) =
      printTextLn $ "Error! Invalid column: " <> showb cn <> ". Select only from [1-7]."

main :: IO ()
main = do
  let config@Config{..} = Config{numOfCols = 7, numOfRows = 6, winLength = 4}
      newEmptyBoard = emptyBoard config :: SimpleBoard
      game = showBoard >>= liftIO . T.putStrLn >>  play
  rs <- (fmap.fmap) fst . runExceptT . flip runStateT newEmptyBoard . runReaderT game $ config
  case rs of
    Left error -> T.putStrLn "Unexpected Error!"
    Right _ -> T.putStrLn "Good Bye!"
