module Database.Kawa.Store
  ( Store(..)
  , Key(..)
  , Value(..)
  , emptyStore
  , set
  , get
  , toText
  , fromText
  , readStore
  , readStore'
  , writeStore
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
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TL

newtype Key   = Key   { fromKey   :: T.Text } deriving (Eq, Show, Hashable)
newtype Value = Value { fromValue :: T.Text } deriving (Eq, Show)

newtype Store = Store { unStore :: (HM.HashMap Key Value) } deriving (Eq, Show)

emptyStore :: Store
emptyStore = Store HM.empty

set :: Store -> Key -> Value -> Store
set (Store m) k v = Store $ HM.insert k v m

get :: Store -> Key -> Maybe Value
get (Store m) k = HM.lookup k m

toText :: Store -> TL.Text
toText =
    TLB.toLazyText . (<> "\n") . mconcat . intersperse (TLB.singleton '\n')
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
      _ <- some endOfLine
      endOfInput
      checkDuplicates $ map (fromKey . fst) pairs
      return $ Store $ HM.fromList pairs

    checkDuplicates :: [T.Text] -> Parser ()
    checkDuplicates =
      void . foldM (\seen x -> if HS.member x seen
                                 then fail $ "duplicate key: " ++ show x
                                 else return (HS.insert x seen))
                   HS.empty

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

readStore :: FilePath -> IO (Either String Store)
readStore = fmap fromText . TL.readFile

readStore' :: FilePath -> IO Store
readStore' path = do
  eRes <- readStore path
  case eRes of
    Left err -> error $ "can't read store at " ++ path ++ ": " ++ err
    Right store -> return store

writeStore :: FilePath -> Store -> IO ()
writeStore path store = TL.writeFile path $ toText store
