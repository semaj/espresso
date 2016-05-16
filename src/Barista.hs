module Barista (runApp, app) where

import           Control.Monad.IO.Class
import           Data
import           Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import           Data.Char (toLower)
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

 -- yeah, I use the Maybe monad. (⌐■_■)
getCreds :: Maybe String -> Maybe String -> Maybe String ->
            Maybe String -> Maybe PG.ConnectInfo
getCreds username password db host = do
  u <- username
  p <- password
  d <- db
  h <- host
  return $ PG.defaultConnectInfo { PG.connectHost = h
                                 , PG.connectUser = u
                                 , PG.connectPassword = p
                                 , PG.connectDatabase = d
                                 }

filterSize :: String -> Int
filterSize = toInt . (map toLower)
  where toInt "small" = 1000
        toInt "medium" = 100000
        toInt _ = 100000000

connectionPool :: IO (P.Pool PG.Connection)
connectionPool = do
  password <- getEnv "ESPRESSO_DB_PASSWORD"
  username <- getEnv "ESPRESSO_DB_USERNAME"
  database <- getEnv "ESPRESSO_DB_DATABASE"
  host <- getEnv "ESPRESSO_DB_HOST"
  case getCreds username password database host of
    Just creds -> P.createPool (PG.connect creds) PG.close 1 40 10
    Nothing -> error ("Could not get DB environment variables! " ++
                      (show username) ++ " " ++ (show password) ++
                      " " ++ (show host) ++" " ++ (show database))

getFilter :: P.Pool PG.Connection -> Int -> IO (Maybe Filter)
getFilter pool slice = do
  results <- fetch pool (PG.Only slice) "SELECT primary_id, filter_data FROM daily_filters WHERE max_rank = ? LIMIT 1"
  return $ safeHead results

getIsRevoked :: P.Pool PG.Connection -> String -> IO Bool
getIsRevoked pool serialHex = do
  results <- fetch pool (serialHex, serialHex) "SELECT 1 FROM revoked_certs WHERE serial_hex = ? UNION SELECT 1 FROM revoked_from_crls WHERE serial_hex = ?" :: IO [PG.Only Int]
  return $ (length results) >= 1

app' :: P.Pool PG.Connection -> S.ScottyM ()
app' pool = do
  S.get "/todays-filter/:size" $ do
    maxRank <- S.param "size" :: S.ActionM String
    result <- liftIO $ getFilter pool (filterSize maxRank)
    case result of
      Just r -> S.json r
      Nothing -> S.json $ A.Object $ E.fromList [("error", "No filter available.")]

  S.get "/test" $ do
    S.text "success"

  S.get "/is-revoked/:serialHex" $ do
    serialHex <- S.param "serialHex" :: S.ActionM String
    result <- liftIO $ getIsRevoked pool serialHex
    S.json $ A.Object $ E.fromList [("is-revoked", AT.Bool result)]

app :: IO Application
app = do
  pool <- connectionPool
  S.scottyApp (app' pool)

runApp :: IO ()
runApp = do
  pool <- connectionPool
  S.scotty 80 (app' pool)
