{- |
Module      : readDB
Description : Utility to read the course database and provide useful output

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-11-26

-}

module ReadDB where

import qualified Data.ByteString.Char8 as BS
import Text.Printf
import Data.Char

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

data Entry = Entry { idKey :: Int
                   , titleKey :: String
                   , codeKey :: String
                   , instructorsKey :: String
                   , minUnitsKey :: Int
                   , maxUnitsKey :: Int
                   , descriptionKey :: String
                   } deriving (Eq, Ord)

instance Show Entry where
  show entry = (codeKey entry) ++ ": " ++ (titleKey entry)

readDB :: FilePath -> IO [[SqlValue]]
readDB filename =
  do db <- connectSqlite3 filename
     res <- quickQuery' db (printf "SELECT * FROM courseinfo") []
     disconnect db
     return res

fromSqlString :: SqlValue -> String
fromSqlString = map toLower . BS.unpack . fromSql

makeEntry :: [SqlValue] -> Entry
makeEntry [id', title', code', instructors', minUnits', maxUnits', description'] =
  Entry { idKey = fromSql id', titleKey = fromSqlString title', codeKey = fromSqlString code', instructorsKey = fromSqlString instructors', minUnitsKey = fromSql minUnits', maxUnitsKey = fromSql maxUnits', descriptionKey = fromSqlString description' }

makeEntries :: [[SqlValue]] -> [Entry]
makeEntries outerVals = go outerVals []
  where go [] res = res
        go (val:vals) res = go vals ((makeEntry val):res)
