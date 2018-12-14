{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks, reader)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, AppM(..), Env (envDB), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn = reader (dbConn . envDB)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB fromResult withConn = do
  conn <- getDBConn
  a <- liftIO $ withConn conn
  liftEither $ fromResult a

getComments
  :: Topic
  -> App [Comment]
getComments topic = runDB mapComment query
  where
    mapComment = traverse fromDBComment
    query conn = Sql.query conn sql [getTopic topic]
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic t ct = runDB pure query
  where
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    query conn = do
      args <- getArgs
      Sql.execute conn sql args
    getArgs = do
      currentTime <- getCurrentTime
      pure (getTopic t, getCommentText ct, currentTime)

getTopics
  :: App [Topic]
getTopics = runDB mapTopic query
  where
    sql = "SELECT DISTINCT topic FROM comments"
    query conn = Sql.query_ conn sql :: IO [Sql.Only Text]
    mapTopic = traverse (mkTopic . Sql.fromOnly)

deleteTopic
  :: Topic
  -> App ()
deleteTopic t = runDB pure query
  where
    sql = "DELETE FROM comments WHERE topic = ?"
    query conn = Sql.execute conn sql [getTopic t]

-- Go on to 'src/Level07/Core.hs' next.
