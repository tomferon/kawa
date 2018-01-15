import           Control.Exception

import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM

import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error

import           Database.Kawa.Store

import           Kawa.Options

main :: IO ()
main = do
  cmd <- getCommand

  case cmd of
    Set path key value -> do
      checkExistence <- doesFileExist path
      store <- if checkExistence then readStore' path else return emptyStore
      let store' = set store key value
      writeStore path store'

    Get path key -> do
      store <- readStore' path
      case get store key of
        Nothing -> do
          hPutStrLn stderr "Key not found in store."
          exitFailure
        Just value -> T.putStr $ fromValue value

    Replace path mInputPath mOutputPath -> do
      store <- readStore' path

      -- Considering this is an executable which runs once, we don't need to
      -- care too much about not closing some handles in case of error.
      input  <- maybe (return stdin)  (flip openFile ReadMode)  mInputPath
      output <- maybe (return stdout) (flip openFile WriteMode) mOutputPath

      let pairs = sortBy (compare `on` T.length . fromKey . fst)
                  . filter (not . T.null . fromKey . fst)
                  . HM.toList . unStore $ store

      replace pairs input output

      hClose input
      hClose output

replace :: [(Key, Value)] -> Handle -> Handle -> IO ()
replace pairs input output =
  handle (\e -> if isEOFError e then return () else throw e) $ do
    line <- T.hGetLine input
    let line' = foldl (\l (Key k, Value v) -> T.replace k v l) line pairs
    T.hPutStrLn output line'
    replace pairs input output
