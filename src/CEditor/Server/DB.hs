module CEditor.Server.DB where


--import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe


initDB :: Maybe FilePath -> IO Connection
initDB dbFile = do
  conn <- connectSqlite3 $ fromMaybe ":memory:" dbFile
  
  return conn






