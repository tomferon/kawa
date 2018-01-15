module Database.Kawa.StoreTests
  ( storeProps
  ) where

import           Data.Either (isLeft)
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Database.Kawa.Store

import           TestHelpers

storeProps :: [(PropertyName, Property)]
storeProps = groupProperties $$(discover)

prop_setThenGetReturnsSetValue :: Property
prop_setThenGetReturnsSetValue = property $ do
  store <- forAll storeGen
  key   <- forAll keyGen
  value <- forAll valueGen
  get (set store key value) key === Just value

prop_toTextThenFromTextReturnsInitialStore :: Property
prop_toTextThenFromTextReturnsInitialStore = property $ do
  store <- forAll storeGen
  annotate $ TL.unpack $ toText store
  fromText (toText store) === Right store

prop_fromTextFailsIfThereAreDuplicateKeys :: Property
prop_fromTextFailsIfThereAreDuplicateKeys = property $ do
  txt <- forAll $ do
    key    <- Gen.text (Range.linear 0 10) Gen.alpha
    value1 <- Gen.text (Range.linear 0 10) Gen.alpha
    value2 <- Gen.text (Range.linear 0 10) Gen.alpha
    return $ TL.fromStrict $
      key <> " = " <> value1 <> "\n" <> key <> " = " <> value2

  let eRes = fromText txt
  annotateShow eRes
  assert $ isLeft eRes
