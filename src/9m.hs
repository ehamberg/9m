{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Acid
import Data.Char
import Network.HTTP.Types.Status
import System.Random
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Web.Scotty hiding (get, put)
import qualified Data.Map as Map
import Data.Text.Lazy hiding (find)
import qualified Web.Scotty as Scotty

import DataLayer

getIndexH :: ActionM ()
getIndexH = html $ renderHtml [shamlet|
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>
      9m – A URL Shortener for the Unicode Age
  <body>
    <p>
      <form method="post" action="/create">
        URL to shorten:
        <input type="text" name="url">
|]

postCreateH :: AcidState KeyValue -> ActionM ()
postCreateH db = do
    u <- prefixHttp `fmap` param "url"
    key <- liftIO $ do
      existing <- find db u
      case existing of
        Just v  -> return v
        Nothing -> do
          k <- randomKey 2
          insert db k u
          insert db u k
          return k
    redirect $ "/show/" `append` key
  where prefixHttp url
          | "http://" `isPrefixOf` url  = url
          | "https://" `isPrefixOf` url = url
          | otherwise = "http://" `append` url

randomKey :: Int -> IO Text
randomKey n = liftM pack $ replicateM n randomPrintChar
  where randomPrintChar = do
            c <- randomRIO ('A', '\128709')
            if isPrint c
              then return c
              else randomPrintChar

getRedirectH :: AcidState KeyValue -> ActionM ()
getRedirectH db = do
    key <- param "key"
    mbVal <- liftIO $ find db key
    case mbVal of
      Nothing    -> status status404
      Just value -> redirect' value
  where redirect' url = do
          setHeader "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
          setHeader "pragma" "no-cache"
          setHeader "location" url
          status status301

getShowH :: AcidState KeyValue -> ActionM ()
getShowH db = do
    key <- param "key"
    mbVal <- liftIO $ find db key
    case mbVal of
      Nothing    -> status status404
      Just value -> html $ renderHtml [shamlet|
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>
      9m – A URL Shortener for the Unicode Age
  <body>
    <p>
      <a href="http://9m.no/#{key}">
        http://9m.no/#{key}
      →
      <a href="#{value}">
        #{value}
|]

nineM :: AcidState KeyValue -> ScottyM ()
nineM db = do
  Scotty.get  "/"          getIndexH
  Scotty.get  "/:key"      (getRedirectH db)
  Scotty.get  "/show/:key" (getShowH db)
  Scotty.post "/create"    (postCreateH db)

main :: IO ()
main = bracket
  (openLocalState (KeyValue Map.empty))
  closeAcidState
  (scotty 7000 . nineM)
