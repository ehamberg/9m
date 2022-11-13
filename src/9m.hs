{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Char qualified
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T (any, isInfixOf, isPrefixOf, length, pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import DataLayer (ConnectionPool, findByKey, findByUrl, initialize, insert, recordHit)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types (StdMethod (GET, POST), status301, status400, status404, urlEncode)
import System.Random (randomRIO)
import Templates (aboutTpl, indexTpl, selfTpl, showTpl)
import Web.Scotty
import Prelude

getIndexH :: ActionM ()
getIndexH = html indexTpl

getAboutH :: ActionM ()
getAboutH = html aboutTpl

postCreateH :: ConnectionPool -> ActionM ()
postCreateH pool = do
  u <- prefixHttp `fmap` param "url"
  if
      | T.length u > 500 || T.any (< ' ') u -> badRequest
      | "data:" `T.isInfixOf` u -> badRequest
      | "http://9m.no" `T.isPrefixOf` u -> redirect "/self"
      | "https://9m.no" `T.isPrefixOf` u -> redirect "/self"
      | otherwise -> insertAndRedirect u pool
  where
    badRequest = status status400 >> text "Bad request"
    prefixHttp url
      | "http://" `T.isPrefixOf` url = url
      | "https://" `T.isPrefixOf` url = url
      | otherwise = "http://" <> url

insertAndRedirect :: Text -> ConnectionPool -> ActionM ()
insertAndRedirect url pool = do
  key <- liftIO $ do
    existing <- findByUrl pool url
    case existing of
      Just v -> return v
      Nothing -> do
        k <- randomKey 2
        insert pool k url
        return k
  redirect $ "/show/" <> (cs . urlEncode False . cs . encodeUtf8 $ key)

randomKey :: Int -> IO Text
randomKey n = T.pack <$> replicateM n randomPrintChar
  where
    randomPrintChar = do
      c <- randomRIO ('A', '\128709')
      if Data.Char.isPrint c
        then return c
        else randomPrintChar

getRedirectH :: ConnectionPool -> ActionM ()
getRedirectH pool = do
  key <- param "key"
  mbVal <- liftIO $ findByKey pool key
  liftIO $ recordHit pool key
  case mbVal of
    Nothing -> status status404
    Just value -> performRedirect value
  where
    performRedirect url = do
      setHeader "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
      setHeader "pragma" "no-cache"
      setHeader "location" url
      status status301

getShowH :: ConnectionPool -> ActionM ()
getShowH pool = do
  key <- param "key" `rescue` const next
  mbVal <- liftIO $ findByKey pool key
  case mbVal of
    Nothing -> status status404
    Just value -> html $ showTpl key value

getSelfH :: ActionM ()
getSelfH = html selfTpl

nineM :: ConnectionPool -> ScottyM ()
nineM pool = do
  addroute GET "/" getIndexH
  addroute GET "/about" getAboutH
  addroute GET "/self" getSelfH
  addroute GET "/:key" (getRedirectH pool)
  addroute GET "/show/:key" (getShowH pool)
  addroute POST "/create" (postCreateH pool)

  -- static svg files
  addroute GET "/static/svg/:file" $ do
    setHeader "content-type" "image/svg+xml"
    param "file" >>= file . ("/static/svg/" ++)

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "9m.db" 10 $ \pool ->
  liftIO $ initialize pool >> scotty 7000 (nineM pool)
