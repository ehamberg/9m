{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataLayer
  ( initialize
  , insert
  , findByKey
  , findByUrl
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
initialize pool = flip DB.runSqlPool pool $ DB.runMigration migrateAll

findByKey :: DB.ConnectionPool -> T.Text -> IO (Maybe T.Text)
findByKey pool key = flip DB.runSqlPool pool $ do
  url <- DB.getBy $ UniqueKey key
  return $ case url of
                Just (DB.Entity _ v) -> Just $ mappingUrl v
                Nothing -> Nothing

findByUrl :: DB.ConnectionPool -> T.Text -> IO (Maybe T.Text)
findByUrl pool url = flip DB.runSqlPool pool $ do
  url' <- DB.getBy $ UniqueUrl url
  return $ case url' of
                Just (DB.Entity _ v) -> Just $ mappingShortcut v
                Nothing -> Nothing

insert :: DB.ConnectionPool -> T.Text -> T.Text -> IO ()
insert pool key url = flip DB.runSqlPool pool $ do
  t <- liftIO getCurrentTime
  _ <- DB.insert $ Mapping key url t
  return ()
