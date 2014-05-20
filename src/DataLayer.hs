{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module DataLayer
  ( KeyValue (..)
  , insert
  , find
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T

type Key = T.Text
type Url = T.Text

data KeyValue = KeyValue !(Map.Map Key Url)
  deriving (Typeable, Show)
$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: Key -> Url -> Update KeyValue ()
insertKey key value = do
    KeyValue m <- get
    put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Url)
lookupKey key = do
    KeyValue m <- ask
    return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

insert :: AcidState KeyValue -> Key -> Url -> IO ()
insert acid key value = update acid (InsertKey key value)

find :: AcidState KeyValue -> Key -> IO (Maybe T.Text)
find acid key = query acid (LookupKey key)
