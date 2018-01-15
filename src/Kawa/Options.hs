module Kawa.Options
  ( Command(..)
  , getCommand
  ) where

import           Data.Monoid
import qualified Data.Text as T

import           Options.Applicative

import           Database.Kawa.Store

data Command
  = Set FilePath Key Value
  | Get FilePath Key
  | Replace FilePath (Maybe FilePath) (Maybe FilePath)

getCommand :: IO Command
getCommand = execParser $
    info (parser <**> helper)
         (fullDesc <> progDesc "Manage key-value stores in single files")

  where
    parser :: Parser Command
    parser = hsubparser $
      command "set" (info setParser
                     (progDesc "Set a key-value pair in a store"))
      <> command "get" (info getParser
                        (progDesc "Get the value of a key from a store"))
      <> command "replace" (info replaceParser
                            (progDesc "Replace keys by their value in a file\
                                      \ (line by line)"))

    setParser :: Parser Command
    setParser = Set
      <$> strArgument (metavar "PATH" <> help "Path to the store file")
      <*> argument (fmap (Key . T.pack) str)
                   (metavar "KEY" <> help "The key to set")
      <*> argument (fmap (Value . T.pack) str)
                   (metavar "VALUE" <> help "The value to set it to")

    getParser :: Parser Command
    getParser = Get
      <$> strArgument (metavar "PATH" <> help "Path to the store file")
      <*> argument (fmap (Key . T.pack) str)
                   (metavar "KEY" <> help "The key to set")

    replaceParser :: Parser Command
    replaceParser = Replace
      <$> strArgument (metavar "STORE" <> help "Path to the store file")
      <*> option (fmap Just str)
                 (short 'i' <> long "input" <> metavar "INPUT" <> value Nothing
                  <> help "Path to the input file (default: stdin)")
      <*> option (fmap Just str)
                 (short 'o' <> long "output" <> metavar "OUTPUT"
                  <> value Nothing
                  <> help "Path to the output file (default: stdout)")
