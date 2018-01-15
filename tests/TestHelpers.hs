module TestHelpers where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Database.Kawa.Store

storeGen :: Gen Store
storeGen = do
  pairs <- Gen.list (Range.linear 0 10) $ (,) <$> keyGen <*> valueGen
  return $ Store $ HM.fromList pairs

keyGen :: Gen Key
keyGen = Key <$> stringGen

valueGen :: Gen Value
valueGen = Value <$> stringGen

stringGen :: Gen T.Text
stringGen = Gen.text (Range.linear 0 100) $ Gen.frequency
  [ (10, Gen.ascii)
  , (1,  Gen.unicodeAll)
  , (1,  Gen.element "= \r\n\t\\\"\'")
  ]
