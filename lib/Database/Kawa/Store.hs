module Database.Kawa.Store
  ( Store(..)
  , Key(..)
  , Value(..)
  , set
  , get
  , toText
  , fromText
  ) where

import           Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Monad

import           Data.Attoparsec.Text.Lazy ( Parser, eitherResult, parse, sepBy
                                           , endOfLine, takeTill, takeWhile
                                           , skipWhile, char, anyChar
                                           , isHorizontalSpace, endOfInput )
import           Data.Char (isAlphaNum)
import           Data.Hashable (Hashable)
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

newtype Key   = Key   { fromKey   :: T.Text } deriving (Eq, Show, Hashable)
newtype Value = Value { fromValue :: T.Text } deriving (Eq, Show)

newtype Store = Store { unStore :: (HM.HashMap Key Value) } deriving (Eq, Show)

set :: Store -> Key -> Value -> Store
set (Store m) k v = Store $ HM.insert k v m

get :: Store -> Key -> Maybe Value
get (Store m) k = HM.lookup k m

toText :: Store -> TL.Text
toText =
    TLB.toLazyText . mconcat . intersperse (TLB.singleton '\n')
    . map formatPair . HM.toList . unStore

  where
    formatPair :: (Key, Value) -> TLB.Builder
    formatPair (Key k, Value v) =
      formatString k <> TLB.fromText " = " <> formatString v

    formatString :: T.Text -> TLB.Builder
    formatString txt
      | T.all isAuthorizedChar txt = TLB.fromText txt
      | otherwise = TLB.fromString (show txt)

fromText :: TL.Text -> Either String Store
fromText = eitherResult . parse parser
  where
    parser :: Parser Store
    parser = do
      pairs <- pairParser `sepBy` some endOfLine
      endOfInput
      return $ Store $ HM.fromList pairs

    pairParser :: Parser (Key, Value)
    pairParser = do
      skipHorizontalSpace
      k <- stringParser
      skipHorizontalSpace
      _ <- char '='
      skipHorizontalSpace
      v <- stringParser
      skipHorizontalSpace
      return (Key k, Value v)

    stringParser :: Parser T.Text
    stringParser = escapedStringParser <|> plainStringParser

    escapedStringParser :: Parser T.Text
    escapedStringParser = do
      c <- anyChar
      guard $ c == '"'
      str <- (\f -> '"' : f []) <$> consumeEscapedString id
      case reads str of
        (x,""):_ -> return x
        _ -> fail $ "can't parse string: " ++ str

    plainStringParser :: Parser T.Text
    plainStringParser = takeWhile isAuthorizedChar

    consumeEscapedString :: (String -> String) -> Parser (String -> String)
    consumeEscapedString acc = do
      chunk <- T.unpack <$> takeTill (\c -> c == '"' || c == '\\')
      c <- anyChar
      case c of
        '"' -> return $ acc . (chunk ++) . (c :)
        '\\' -> do
          c' <- anyChar
          consumeEscapedString $ acc . (chunk ++) . (c :) . (c' :)
        _ -> fail "the impossible happened"

    skipHorizontalSpace :: Parser ()
    skipHorizontalSpace = skipWhile isHorizontalSpace

isAuthorizedChar :: Char -> Bool
isAuthorizedChar c = isAlphaNum c || c == '-' || c == '_'
