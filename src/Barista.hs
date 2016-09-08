module Barista (runApp, app) where

import qualified Codec.Compression.Zlib as Z
import           Control.Monad.IO.Class
import           Data
import           Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Bits as BIT
import           Data.Bits.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import           Data.Char (toLower)
import           Data.List as L
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
        toInt _ = 10000000

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

getFilter :: P.Pool PG.Connection -> Int -> IO (Maybe (PG.Only BL.ByteString))
getFilter pool slice = do
  results <- fetch pool (PG.Only slice) "SELECT filter_bytes FROM filters WHERE max_rank = ? AND fp_rate = 0.001 ORDER BY inserted DESC LIMIT 1" :: IO [PG.Only BL.ByteString]
  return $ safeHead results

foldDiffs :: [BL.ByteString] -> BL.ByteString
foldDiffs = Z.compress . L.foldl' (\acc x -> (BIT.xor acc $ Z.decompress x)) (BL.repeat 0)

getDiff :: P.Pool PG.Connection -> Int -> Int -> IO BL.ByteString
getDiff pool slice currentID = do
  results <- fetch pool (slice, currentID) "SELECT diff_bytes FROM diffs WHERE max_rank = ? AND old_filter >= ? ORDER BY inserted DESC LIMIT 1" :: IO [PG.Only BL.ByteString]
  let folded = foldDiffs $ map PG.fromOnly results
  return folded

getMeta :: P.Pool PG.Connection -> Int -> IO (Maybe (PG.Only Int))
getMeta pool slice = do
  results <- fetch pool (PG.Only slice) "SELECT primary_id FROM filters WHERE max_rank = ? ORDER BY inserted DESC LIMIT 1"
  return $ safeHead results

getIsRevoked :: P.Pool PG.Connection -> String -> IO Bool
getIsRevoked pool identifier = do
  results <- fetch pool (identifier, identifier) "SELECT 1 FROM revoked_certs WHERE identifier = ? UNION SELECT 1 FROM revoked_from_crls WHERE identifier = ?" :: IO [PG.Only Int]
  return $ (length results) >= 1

app' :: P.Pool PG.Connection -> S.ScottyM ()
app' pool = do
  S.get "/todays-meta/:size" $ do
    maxRank <- S.param "size" :: S.ActionM String
    result <- liftIO $ getMeta pool (filterSize maxRank)
    case result of
      Just r -> S.json $ A.Object $ E.fromList [("primary_id", AT.toJSON $ PG.fromOnly r)]
      Nothing -> S.json $ A.Object $ E.fromList [("error", "No filter available.")]
  S.get "/todays-filter/:size" $ do
    maxRank <- S.param "size" :: S.ActionM String
    result <- liftIO $ getFilter pool (filterSize maxRank)
    case result of
      Just r -> S.raw $ PG.fromOnly r
      Nothing -> S.json $ A.Object $ E.fromList [("error", "No filter available.")]
  S.get "/diff/:size" $ do
    maxRank <- S.param "size" :: S.ActionM String
    currentID <- S.param "current_id" :: S.ActionM Int
    result <- liftIO $ getDiff pool (filterSize maxRank) currentID
    S.raw result

  S.get "/" $ do
    S.text "hello!"

  S.get "/is-revoked/:identifier" $ do
    identifier <- S.param "identifier" :: S.ActionM String
    result <- liftIO $ getIsRevoked pool identifier
    S.json $ A.Object $ E.fromList [("is-revoked", AT.Bool result)]

app :: IO Application
app = do
  pool <- connectionPool
  S.scottyApp (app' pool)

runApp :: IO ()
runApp = do
  pool <- connectionPool
  S.scotty 8080 (app' pool)
