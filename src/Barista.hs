module Barista (runApp, app) where

import           Control.Monad.IO.Class
import           Data
import           Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.Aeson as A
import qualified Data.Pool as P
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.Simple as PG
import qualified GHC.Exts as E
import           Network.Wai (Application)
import           System.Posix.Env
import           Utils
import qualified Web.Scotty as S

fetch :: (PG.FromRow r, PG.ToRow q) => P.Pool PG.Connection -> q -> PG.Query -> IO [r]
fetch pool args sql = P.withResource pool retrieve
  where retrieve conn = PG.query conn sql args

connectionPool :: IO (P.Pool PG.Connection)
connectionPool = do
  password <- getEnvDefault "ESPRESSO_DB_PASSWORD" ""
  username <- getEnvDefault "ESPRESSO_DB_USERNAME" ""
  database <- getEnvDefault "ESPRESSO_DB_DATABASE" ""
  host <- getEnvDefault "ESPRESSO_DB_HOST" ""
  let info = PG.defaultConnectInfo { PG.connectHost = host
                                   , PG.connectUser = username
                                   , PG.connectPassword = password
                                   , PG.connectDatabase = database
                                   }
  P.createPool (PG.connect info) PG.close 1 40 10

getFilter :: P.Pool PG.Connection -> Int -> IO (Maybe Filter)
getFilter pool slice = do
  results <- fetch pool (PG.Only slice) "SELECT primary_id, filter_data FROM daily_filters WHERE max_rank = ? LIMIT 1"
  return $ safeHead results

app' :: P.Pool PG.Connection -> S.ScottyM ()
app' pool = do
  S.get "/todays-filter/:max-rank" $ do
    maxRank <- S.param "max-rank" :: S.ActionM Int
    result <- liftIO $ getFilter pool maxRank
    case result of
      Just r -> S.json result
      Nothing -> S.json $ A.Object $ E.fromList [("error", "No filter available.")]

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
