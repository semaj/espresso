module Barista (runApp, app) where

import qualified Constants as C
import           Control.Monad.IO.Class
import           Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.Pool as P
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.Simple as PG
import           Network.Wai (Application)
import qualified Web.Scotty as S

fetch :: (PG.FromRow r, PG.ToRow q) => P.Pool PG.Connection -> q -> PG.Query -> IO [r]
fetch pool args sql = P.withResource pool retrieve
  where retrieve conn = PG.query conn sql args

getFilter :: P.Pool PG.Connection -> Int -> IO [Value]
getFilter pool slice = do
  results <- fetch pool (PG.Only slice) "SELECT filter_data FROM daily_filters WHERE max_rank <= ?"
  return $ map PG.fromOnly results

connectionPool :: IO (P.Pool PG.Connection)
connectionPool = P.createPool (PG.connect C.dbCreds) PG.close 1 40 10

app' :: P.Pool PG.Connection -> S.ScottyM ()
app' pool = do
  S.get "/todays-filter/:max-rank" $ do
    maxRank <- S.param "max-rank" :: S.ActionM Int
    result <- liftIO $ getFilter pool maxRank
    S.json result

  S.get "/test" $ do
    S.text "success"

app :: IO Application
app = do
  pool <- connectionPool
  S.scottyApp (app' pool)

runApp :: IO ()
runApp = do
  pool <- connectionPool
  S.scotty 8080 (app' pool)
