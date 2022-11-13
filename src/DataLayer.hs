{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- To avoid “unused” warnings from `mkpersist`
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module DataLayer
  ( initialize,
    insert,
    findByKey,
    findByUrl,
    recordHit,
    ConnectionPool,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql ((+=.), (==.))
import Database.Persist.Sql qualified as DB
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

type ConnectionPool = DB.ConnectionPool

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Mapping json
  shortcut T.Text
  url T.Text
  hits Int default=0
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

recordHit :: DB.ConnectionPool -> T.Text -> IO ()
recordHit pool key = flip DB.runSqlPersistMPool pool $ do
  DB.updateWhere [MappingShortcut ==. key] [MappingHits +=. 1]

insert :: DB.ConnectionPool -> T.Text -> T.Text -> IO ()
insert pool key url = flip DB.runSqlPool pool $ do
  t <- liftIO getCurrentTime
  _ <- DB.insert $ Mapping key url 0 t
  return ()
