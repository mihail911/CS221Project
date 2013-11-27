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
import Data.List
import qualified Data.Map as Map

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

import Util

smallDBName = "courseinfo-small.db"

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

-- | Get a reduced cross section of the database that will be easier
-- to work with.
getDBCrossSection :: [Entry] -> [Entry]
getDBCrossSection = filter p
  where p entry = minUnitsKey entry > 2
                  && any (`isPrefixOf` (codeKey entry))
                     ["cs", "math", "jewishst", "amelang"]
                  && number <= 150
          where number :: Int
                number = read $ filter isDigit $ codeKey entry

writeDB :: FilePath -> [Entry] -> IO ()
writeDB filename entries =
  do db <- connectSqlite3 filename
     quickQuery' db "DROP TABLE courseinfo" []
     quickQuery' db "CREATE TABLE courseinfo (id INTEGER PRIMARY KEY NOT NULL,title NOT NULL,code NOT NULL,instructor NOT NULL,unitsmin INTEGER NOT NULL,unitsmax INTEGER NOT NULL,description)" []
     stmt <- prepare db "INSERT INTO courseinfo VALUES (?,?,?,?,?,?,?)"
     executeMany stmt $ map (\entry -> [toSql (idKey entry),
                                        toSql (titleKey entry),
                                        toSql (codeKey entry),
                                        toSql (instructorsKey entry),
                                        toSql (minUnitsKey entry),
                                        toSql (maxUnitsKey entry),
                                        toSql (descriptionKey entry)])
       entries
     commit db
     disconnect db
     return ()

writeRelatednessGraph :: RelatednessGraph -> IO ()
writeRelatednessGraph graph =
  do db <- connectSqlite3 smallDBName
     -- quickQuery' db "DROP TABLE relatedness" []
     quickQuery' db "CREATE TABLE relatedness (id INTEGER NOT NULL,related_id INTEGER NOT NULL)" []
     stmt <- prepare db "INSERT INTO relatedness VALUES (?,?)"
     executeMany stmt $ concatMap mapper $ Map.toList graph
     commit db
     disconnect db
     return ()
  where mapper (entry, relateds) = map (\r -> [toSql (idKey entry), toSql (idKey r)])
                                   relateds

makeSmallDB :: IO ()
makeSmallDB =
  do db <- readDB "courseinfodata.db"
     let entries = makeEntries db
     print $ length $ getDBCrossSection entries
     writeDB smallDBName (getDBCrossSection entries)
