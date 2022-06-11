{-# LANGUAGE OverloadedStrings #-}
module ByteCode.Parser where

import TextShow
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit, isLower, isSpace, isUpper)
import GHC.Unicode (isAlpha)
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad.Except
import Control.Monad.Identity

import ByteCode.Data

data SError
  = INVALID_CODE Text
  | INVALID_NUMBER Text
  | INVALID_LABEL Text
  | INVALID_VAR Text

instance TextShow SError where
  showb (INVALID_CODE l) = "Either *operator* is invalid or it is followed by extra/invalid characters in line [" <> fromText l <> "]"
  showb (INVALID_NUMBER l) = "Either *integer* is invalid or extra spaces in line [" <> fromText l <> "]"
  showb (INVALID_LABEL l) = "Either *label* is invalid(chars allowed [A-Z]) or extra spaces in line [" <> fromText l <> "]"
  showb (INVALID_VAR l) = "Either *variable* is invalid(chars allowed [a-z]) or extra spaces in line [" <> fromText l <> "]"

type ParserResult = ExceptT SError Identity

pInteger :: RP.ReadP Integer
pInteger =
  let nat = read <$> RP.many1 (RP.satisfy isDigit)
   in do
        s <- RP.look
        case s of
          ('-' : ss) -> fmap (\n -> (- n)) (RP.char '-' *> nat)
          _ -> nat

parser :: Text -> ParserResult [ByteCode]
parser txt = traverse parseLine (filter nonEmptyLine $ T.lines txt)
  where
    nonEmptyLine ln = not $ T.null ln || T.take 1 ln == "#"
    parseLine :: Text -> ParserResult ByteCode
    parseLine line = rs
      where
        (oprtr, extra) = T.span (not . isSpace) line
        parse serr parser = case RP.readP_to_S parser (T.unpack extra) of
          (x, _) : _ -> pure x
          _ -> throwError (serr line)
        pVal = RP.char ' ' *> pInteger <* RP.eof
        pVar = fmap T.pack $ RP.char ' ' *> RP.string "â€˜" *> RP.many1 (RP.satisfy (\ch -> isAlpha ch && isLower ch)) <* RP.string "â€™" <* RP.eof
        pLbl = RP.many1 (RP.satisfy (\ch -> isAlpha ch && isUpper ch))
        pLbl' = fmap T.pack $ RP.char ' ' *> pLbl <* RP.eof
        rs = case oprtr of
          "LOAD_VAL" -> LOAD <$> parse INVALID_NUMBER pVal
          "WRITE_VAR" -> WRITE <$> parse INVALID_VAR pVar
          "READ_VAR" -> READ <$> parse INVALID_VAR pVar
          "CJUMP" -> CJUMP <$> parse INVALID_LABEL pLbl'
          "ADD" -> ADD <$ parse INVALID_CODE RP.eof
          "MULTIPLY" -> MULT <$ parse INVALID_CODE RP.eof
          "SUBTRACT" -> SUB <$ parse INVALID_CODE RP.eof
          "DIVIDE" -> DIV <$ parse INVALID_CODE RP.eof
          "RETURN_VALUE" -> RETURN <$ parse INVALID_CODE RP.eof
          lbl | not (T.null lbl) && T.last lbl == ':' ->
                if not (T.null inits) && T.all isUpper inits && T.length extra == 0 then
                  pure (LABEL inits)
                else
                  throwError $ INVALID_LABEL line where inits = T.init lbl
          _ -> throwError $ INVALID_CODE line
