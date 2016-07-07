module Data where

import           Data.Aeson
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified GHC.Exts as E

-- Index of bloom filter bit array (as string) => new value
type FilterDiff = M.Map String Int

coalesce :: [FilterDiff] -> FilterDiff
coalesce = foldl' M.union M.empty

filterDiffJson :: [FilterDiff] -> Value
filterDiffJson fds = Object $ E.fromList [("filter_diff", toJSON $ coalesce fds)]

