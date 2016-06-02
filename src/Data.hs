module Data where

import           Data.Aeson
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified GHC.Exts as E

data Filter = Filter { filterID :: Int
                     , filterData :: B.ByteString
                     }

-- Index of bloom filter bit array (as string) => new value
type FilterDiff = M.Map String Int

coalesce :: [FilterDiff] -> FilterDiff
coalesce = foldl' M.union M.empty

filterDiffJson :: [FilterDiff] -> Value
filterDiffJson fds = Object $ E.fromList [("filter_diff", toJSON $ coalesce fds)]

instance ToJSON Filter where
  toJSON Filter{..} = object [
    "filter_id" .= filterID,
    "filter_data" .= (B.unpack filterData)
    ]

instance PG.FromRow Filter where
  fromRow = Filter <$> PG.field <*> PG.field
