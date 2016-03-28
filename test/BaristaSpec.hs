module BaristaSpec where

import Data.Aeson (Value(..), object, (.=))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Barista (app)

spec :: Spec
spec = with app $ do
  describe "GET /test" $ do
    it "responds with 200" $ do
      get "/test" `shouldRespondWith` 200

    it "responds with 'hello'" $ do
      get "/test" `shouldRespondWith` "success"

    it "responds with 200 / 'success'" $ do
      get "/test" `shouldRespondWith` "success" { matchStatus = 200 }

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      get "/test" `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"] }

  describe "GET /todays-filter/1000" $ do
    it "responds with some JSON" $ do
      get "/todays-filter/1000" `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"] }
