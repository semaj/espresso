module DataSpec where

import           Data
import           Data.Aeson
import qualified Data.Map.Strict as M
import           Test.Hspec
import qualified Test.Hspec.Wai.JSON as J

spec :: Spec
spec = do
  let fd1 = M.fromList [("1", 1), ("2", 2)]
      fd2 = M.fromList [("1", 3), ("3", 3)]
      combined = M.fromList [("1", 1), ("2", 2), ("3", 3)]

  describe "coalesce" $ do
    it "should favor earlier filter diffs" $ do
      coalesce [fd1, fd2] `shouldBe` combined

  describe "filterDiffJson" $ do
    it "should encode as json correctly" $ do
      encode (filterDiffJson [fd1, fd2]) `shouldBe` [J.json| {"filter_diff": {"1": 1, "2": 2,"3": 3 } } |]
