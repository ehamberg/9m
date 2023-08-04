{-# LANGUAGE OverloadedStrings #-}

module SafeBrowsing (checkUrl) where

import Control.Lens
import Data.Aeson
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.Wreq

data SafeBrowsingClient = SafeBrowsingClient
  { clientId :: Text,
    clientVersion :: Text
  }
  deriving (Generic, Show)

instance ToJSON SafeBrowsingClient

newtype SafeBrowsingThreatEntry = SafeBrowsingThreatEntry
  { url :: Text
  }
  deriving (Generic, Show)

instance ToJSON SafeBrowsingThreatEntry

data SafeBrowsingThreatInfo = SafeBrowsingThreatInfo
  { threatTypes :: [Text],
    platformTypes :: [Text],
    threatEntryTypes :: [Text],
    threatEntries :: [SafeBrowsingThreatEntry]
  }
  deriving (Generic, Show)

instance ToJSON SafeBrowsingThreatInfo

instance FromJSON SafeBrowsingThreatEntry

data SafeBrowsingRequest = SafeBrowsingRequest
  { client :: SafeBrowsingClient,
    threatInfo :: SafeBrowsingThreatInfo
  }
  deriving (Generic, Show)

instance ToJSON SafeBrowsingRequest

data SafeBrowsingMatch = SafeBrowsingMatch
  { cacheDuration :: Text,
    threatType :: Text,
    platformType :: Text,
    threatEntryType :: Text,
    threat :: SafeBrowsingThreatEntry
  }
  deriving (Generic, Show)

instance FromJSON SafeBrowsingMatch

newtype SafeBrowsingResponse = SafeBrowsingResponse
  { matches :: Maybe [SafeBrowsingMatch]
  }
  deriving (Generic, Show)

instance FromJSON SafeBrowsingResponse

checkUrl :: Text -> Text -> IO (Either String ())
checkUrl apiKey url = do
  let apiUrl = "https://safebrowsing.googleapis.com/v4/threatMatches:find?key=" <> cs apiKey
  let req =
        SafeBrowsingRequest
          { client = SafeBrowsingClient "9m" "0.1",
            threatInfo =
              SafeBrowsingThreatInfo
                { threatTypes = ["MALWARE", "SOCIAL_ENGINEERING", "UNWANTED_SOFTWARE", "POTENTIALLY_HARMFUL_APPLICATION"],
                  platformTypes = ["ANY_PLATFORM"],
                  threatEntryTypes = ["URL"],
                  threatEntries = [SafeBrowsingThreatEntry url]
                }
          }
  r <- asJSON =<< post apiUrl (toJSON req)
  if null $ matches $ r ^. responseBody
    then pure $ Right ()
    else do
      print (r ^. responseBody)
      pure $ Left $ "URL is not safe:" <> cs url
