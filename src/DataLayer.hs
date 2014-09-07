{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module DataLayer
  ( initialize
  , insert
  , find
  , ConnectionPool
  )
where

import qualified Database.Persist.Sql as DB
import Database.Persist.TH
import Data.Time
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)

type ConnectionPool = DB.ConnectionPool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Mapping json
  shortcut T.Text
  url T.Text
  created UTCTime
  UniqueKey shortcut
  UniqueUrl url
  deriving Show
|]

initialize :: DB.ConnectionPool -> IO ()
initialize pool = flip DB.runSqlPersistMPool pool $ DB.runMigration migrateAll

find :: DB.ConnectionPool -> T.Text -> IO (Maybe T.Text)
find pool keyOrUrl = flip DB.runSqlPersistMPool pool $ do
  url <- DB.getBy $ UniqueKey keyOrUrl
  case url of
       Just (DB.Entity _ v) -> return $ Just $ mappingUrl v
       Nothing -> do
         key <- DB.getBy $ UniqueUrl keyOrUrl
         case key of
              Just (DB.Entity _ v) -> return $ Just $ mappingShortcut v
              Nothing -> return Nothing

insert :: DB.ConnectionPool -> T.Text -> T.Text -> IO ()
insert pool key url = flip DB.runSqlPersistMPool pool $ do
  t <- liftIO getCurrentTime
  _ <- DB.insert $ Mapping key url t
  return ()
