{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Prelude hiding (filter, length, any)
import Control.Monad (liftM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Char
import Network.HTTP.Types
import System.Random (randomRIO)
import Web.Scotty hiding (get, put)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B8
import Data.Text.Lazy hiding (find)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Persist.Sqlite (withSqlitePool)

import DataLayer
import Templates

getIndexH :: ActionM ()
getIndexH = html indexTpl

getAboutH :: ActionM ()
getAboutH = html aboutTpl

postCreateH :: ConnectionPool -> ActionM ()
postCreateH pool = do
    u <- prefixHttp `fmap` param "url"
    if | length u > 500 || any (<' ') u -> badRequest
       | "http://9m.no" `isPrefixOf` u  -> redirect "/self"
       | "https://9m.no" `isPrefixOf` u -> redirect "/self"
       | otherwise                      -> insertAndRedirect u pool
  where badRequest = status status400 >> text "Bad request"
        prefixHttp url
          | "http://" `isPrefixOf` url  = url
          | "https://" `isPrefixOf` url = url
          | otherwise = "http://" `append` url

insertAndRedirect :: Text -> ConnectionPool -> ActionM ()
insertAndRedirect url pool = do
    key <- liftIO $ do
      existing <- findByUrl pool url
      case existing of
        Just v  -> return v
        Nothing -> do
          k <- randomKey 2
          insert pool k url
          return k
    redirect $ "/show/" `append` (pack . B8.unpack . urlEncode False .
                                  LB.toStrict . encodeUtf8 $ key)

randomKey :: Int -> IO Text
randomKey n = liftM pack $ replicateM n randomPrintChar
  where randomPrintChar = do
            c <- randomRIO ('A', '\128709')
            if isPrint c
              then return c
              else randomPrintChar

getRedirectH :: ConnectionPool -> ActionM ()
getRedirectH pool = do
    key <- param "key"
    mbVal <- liftIO $ findByKey pool key
    case mbVal of
      Nothing    -> status status404
      Just value -> performRedirect value
  where performRedirect url = do
          setHeader "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
          setHeader "pragma" "no-cache"
          setHeader "location" url
          status status301

getShowH :: ConnectionPool -> ActionM ()
getShowH pool = do
    key <- param "key" `rescue` const next
    mbVal <- liftIO $ findByKey pool key
    case mbVal of
      Nothing    -> status status404
      Just value -> html $ showTpl key value

getSelfH :: ActionM ()
getSelfH = html selfTpl

nineM :: ConnectionPool -> ScottyM ()
nineM pool = do
  addroute GET  "/"          getIndexH
  addroute GET  "/about"     getAboutH
  addroute GET  "/self"      getSelfH
  addroute GET  "/:key"      (getRedirectH pool)
  addroute GET  "/show/:key" (getShowH pool)
  addroute POST "/create"    (postCreateH pool)

  -- static svg files
  addroute GET "/static/svg/:file" $ do
    setHeader "content-type" "image/svg+xml"
    param "file" >>= file . ("static/svg/" ++)

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "9m.db" 10 $ \pool ->
         liftIO $ initialize pool >> scotty 7000 (nineM pool)
