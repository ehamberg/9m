{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Error (hush)
import Control.Exception (throwIO)
import Control.Monad.Except
import Control.Monad.Logger (runStderrLoggingT)
import Data.Char qualified
import Data.Ini (lookupValue, parseIni)
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import DataLayer (ConnectionPool, findByKey, findByUrl, initialize, insert, recordHit)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types (StdMethod (GET, POST), status301, status400, status404, urlEncode)
import Options.Applicative (Alternative (some, (<|>)), execParser, fullDesc, help, helper, info, long, optional, progDesc, short, strOption, (<**>))
import Options.Applicative qualified as Options
import SafeBrowsing (checkUrl)
import System.Random (randomRIO)
import Templates (aboutTpl, indexTpl, selfTpl, showTpl)
import Web.Scotty
import Prelude

data ConfigSrc
  = FileSrc FilePath
  | DirectSrc Config
  deriving (Show)

data Config = Config
  { safeBrowsingApiKey :: Maybe Text,
    bannedDomains :: [Text]
  }
  deriving (Show)

getIndexH :: ActionM ()
getIndexH = html indexTpl

getAboutH :: ActionM ()
getAboutH = html aboutTpl

postCreateH :: ConnectionPool -> Config -> ActionM ()
postCreateH pool config = do
  u <- prefixHttp `fmap` param "url"
  if
      | TL.length u > 500 || TL.any (< ' ') u -> badRequest
      | "data:" `TL.isInfixOf` u -> badRequest
      | "http://9m.no" `TL.isPrefixOf` u -> redirect "/self"
      | "https://9m.no" `TL.isPrefixOf` u -> redirect "/self"
      | otherwise -> insertAndRedirect u pool config
  where
    badRequest = status status400 >> text "Bad request"
    prefixHttp url
      | "http://" `TL.isPrefixOf` url = url
      | "https://" `TL.isPrefixOf` url = url
      | otherwise = "https://" <> url

insertAndRedirect :: Text -> ConnectionPool -> Config -> ActionM ()
insertAndRedirect url pool config = do
  liftIO (check config url) >>= \case
    Left err -> do
      liftIO $ putStrLn $ "Error: " <> show err
      status status400
      text "Bad request"
    Right () -> do
      key <- liftIO $ do
        existing <- findByUrl pool url
        case existing of
          Just v -> return v
          Nothing -> do
            k <- randomKey 2
            insert pool k url
            return k
      redirect $ "/show/" <> (cs . urlEncode False . cs $ key)

check :: Config -> Text -> IO (Either String ())
check config url = runExceptT $ do
  if isBannedDomain url (bannedDomains config)
    then throwError "Banned domain"
    else case safeBrowsingApiKey config of
      Just apiKey -> do
        res <- liftIO $ checkUrl apiKey url
        case res of
          Left err -> throwError err
          Right () -> pure ()
      Nothing -> pure ()

isBannedDomain :: Text -> [Text] -> Bool
isBannedDomain url bannedDomains = do
  let url' = TL.drop 2 . TL.dropWhile (/= '/') $ url
  any (`TL.isPrefixOf` url') bannedDomains

randomKey :: Int -> IO Text
randomKey n = cs <$> replicateM n randomPrintChar
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

nineM :: ConnectionPool -> Config -> ScottyM ()
nineM pool config = do
  addroute GET "/" getIndexH
  addroute GET "/about" getAboutH
  addroute GET "/self" getSelfH
  addroute GET "/:key" (getRedirectH pool)
  addroute GET "/show/:key" (getShowH pool)
  addroute POST "/create" (postCreateH pool config)

  -- static svg files
  addroute GET "/static/svg/:file" $ do
    setHeader "content-type" "image/svg+xml"
    param "file" >>= file . ("/static/svg/" ++)

directConfig :: Options.Parser Config
directConfig =
  Config
    <$> optional
      ( strOption
          ( long "api-key"
              <> help "SafeBrowsing API Key"
          )
      )
    <*> some
      ( strOption
          ( long "banned"
              <> short 'b'
              <> help "A banned domain"
          )
      )

configSrc :: Options.Parser ConfigSrc
configSrc = DirectSrc <$> directConfig <|> (FileSrc <$> strOption (long "config-file"))

loadConfig :: ConfigSrc -> IO Config
loadConfig (DirectSrc cfg) = pure cfg
loadConfig (FileSrc filePath) = do
  contents <- liftIO (readFile filePath)
  case parseIni (cs contents) of
    Left err -> throwIO (userError err)
    Right ini ->
      let lookupValue' key section = fmap cs . hush $ lookupValue key section ini
          getDomains = either (const (pure [])) $ pure . fmap cs . words . cs
       in Config
            <$> pure (lookupValue' "SAFEBROWSING" "api_key")
            <*> getDomains (lookupValue "NINEM" "banned_domains" ini)

main :: IO ()
main = do
  let opts = info (configSrc <**> helper) (fullDesc <> progDesc "9m Unicode URL shortener")
  config <- execParser opts >>= loadConfig
  print config
  runStderrLoggingT $ withSqlitePool "9m.db" 10 $ \pool ->
    liftIO $ initialize pool >> scotty 7000 (nineM pool config)
