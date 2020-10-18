{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sentry.Types where

import qualified Data.Text as T
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import URI.ByteString
import qualified Data.ByteString as BS
import Lens.Micro
import GHC.Generics
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Char (toLower)
import qualified Data.List as DL
import Network.HTTP.Client as HTTP ( Manager, defaultRequest, Request, method, host, port
                                   , path, queryString, requestHeaders, checkResponse, throwErrorStatusCodes)
import Data.Maybe
import Data.String.Conv (toS, StringConv)
import Network.HTTP.Types.Header as HTTP

newtype EventId = EventId { rawEventId :: UUID } deriving (Eq, Show, Generic)
-- newtype OrgSlug = OrgSlug { rawOrgSlug :: BS.ByteString } deriving (Eq, Show, Generic, FromJSON, ToJSON)
-- newtype AuthToken = AuthToken { rawAuthToken :: BS.ByteString } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToJSON EventId where
  toJSON x = toJSON $ DL.filter (/= '-') $ show $ rawEventId x

{-# INLINE eventIdToHttp #-}
eventIdToHttp :: (StringConv String a) => EventId -> a
eventIdToHttp x = toS $ DL.filter (/= '-') $ show $ rawEventId x

{-# INLINE httpToEventId #-}
httpToEventId :: (StringConv a String) => a -> EventId
httpToEventId s =
  let (x1, (x2, (x3, (x4, x5)))) = (DL.splitAt 8 $ toS s)
                                   & _2 %~ (DL.splitAt 4)
                                   & _2._2 %~ (DL.splitAt 4)
                                   & _2._2._2 %~ (DL.splitAt 4)
  in EventId $ fromJust $ UUID.fromString $ DL.intercalate "-" [x1, x2, x3, x4, x5]


-- type TagName = String
-- type TagValue = String

data Event = Event
  { evtId :: !EventId
  , evtTimestamp :: !UTCTime
  , evtPlatform :: !Platform
  , evtLevel :: !LogLevel
  , evtLogger :: !(Maybe String)
  , evtTransaction :: !(Maybe String)
  , evtServerName :: !(Maybe String)
  , evtRelease :: !(Maybe String)
  , evtDist :: !(Maybe String)
  , evtTags :: ![(String, String)]
  , evtEnvironment :: !(Maybe String)
  , evtModules :: ![(String, String)]
  , evtExtra :: ![(String, String)]
  , evtFingerprint :: ![String]
  -- , evtException :: ![SentryException]
  } deriving (Eq, Show, Generic)

instance ToJSON Event where
  toJSON Event{..} = Aeson.object
    [ "event_id" .= evtId
    , "timestamp" .= evtTimestamp
    , "platform" .= evtPlatform
    , "level" .= evtLevel
    , "logger" .= evtLogger
    , "transaction" .= evtTransaction
    , "server_name" .= evtServerName
    , "release" .= evtRelease
    , "dist" .= evtDist
    , "tags" .= (kvpJson evtTags)
    , "environment" .= evtEnvironment
    , "modules" .= (kvpJson evtModules)
    , "extra" .= (kvpJson evtExtra)
    , "fingerprint" .= evtFingerprint
    ]
    where
      kvpJson kvps = Aeson.object $ DL.map (\(k, v) -> (toS k) .= v) kvps

-- instance {-# OVERLAPS #-} ToJSON [(TagName, TagValue)] where
--   toJSON xs = Aeson.object $ DL.map (\(k, v) -> (toS k) Aeson..= v) xs

data LogLevel = Fatal
              | Error
              | Warning
              | Info
              | Debug
              deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON LogLevel where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions{ Aeson.constructorTagModifier = (DL.map toLower) }

data Platform = PlatformHaskell
              | PlatformOther !T.Text
              deriving (Eq, Show, Generic)

instance ToJSON Platform where
  toJSON x = toJSON $ case x of
    PlatformHaskell -> "haskell"
    PlatformOther y -> y

-- data SentryException = SentryException
--   { exType :: !(Maybe String)
--   , exValue :: !(Maybe String)
--   , exModule :: !(Maybe String)
--   , exThreadId :: !(Maybe ThreadId)
--   , exMechanism :: !_
--   , exStacktrace :: !(Maybe SentryStacktrace)
--   }

-- type ThreadId = String

-- data SentryThread = SentryThread
--   { thId :: !ThreadId
--   , thCrashed :: !(Maybe Boolean)
--   , thCurrent :: !(Maybe Boolean)
--   , thName :: !(Maybe String)
--   , thStacktrace :: !(Maybe SentryStacktrace)
--   }

-- data SentryStacktrace = SentryStacktrace
--   { stFrames :: ![Frame]
--   , stRegisters :: ![(String, String)]
--   }

-- data Frame = Frame
--   { frFilename :: !(Maybe FilePath)
--   , frFunction :: !(Maybe String)
--   , frRawFunction :: !(Maybe String)
--   , frModule :: !(Maybe String)
--   , frLineno :: !(Maybe Int)
--   , frColno :: !(Maybe Int)
--   , frAbsPath :: !(Maybe FilePath)
--   , frContextLine :: !(Maybe String)
--   , frPreContext :: !(Maybe [String])
--   , frPostContext :: !(Maybe [String])
--   , frInApp :: !(Maybe Bool)
--   , frVars :: ![(String, String)]
--   , frInstructionAddr :: ()
--   , frSymbolAddr :: ()
--   , frImageAddr :: ()
--   , frPackage :: !(Maybe String)
--   , frPlatform :: !(Maybe Platform)
--   }

-- -- TODO - make this slightly more type-safe
-- data Message = Message
--   { msgFormatted :: !String
--   , msgMessage :: !(Maybe InterpolationTemplate)
--   , msgParams :: ![InterpolationValues]
--   }

-- type InterpolationValues = String
-- type InterpolationTemplate = String

data SentryConfig = SentryConfig
  { cfgBaseUrl :: !(URIRef Absolute)
  , cfgKey :: !BS.ByteString
  , cfgSecret :: !(Maybe BS.ByteString)
  , cfgProjectId :: !BS.ByteString
  , cfgManager :: !Manager
  , cfgBaseRequest :: !Request
  , cfgAuthToken :: !(Maybe BS.ByteString)
  , cfgOrgSlug :: !(Maybe BS.ByteString)
  }


mkSentryConfig :: Manager
               -> BS.ByteString
               -> Either String SentryConfig
mkSentryConfig mgr x = case parseURI laxURIParserOptions x of
  Left e -> Left $ show e
  Right u ->
    case (u ^? authorityL . _Just . authorityUserInfoL . _Just . uiUsernameL) of
      Nothing -> Left "Secret key is mandatory"
      Just key ->
        let baseUrl = u & (authorityL . _Just . authorityUserInfoL) .~ Nothing
                        & pathL %~ ("/api" <>)
            secret = u ^? authorityL . _Just . authorityUserInfoL . _Just . uiPasswordL
            authComponents = [ ("sentry_version", "7")
                             , ("sentry_client", "better_haskell_raven/1.0")
                             -- TODO: send the timestamp, or not?
                             -- , ("sentry_timestamp", _)
                             , ("sentry_key", key)
                             ] -- <> (maybe [] (\s -> [("sentry_secret", s)]) secret)
            authString = BS.intercalate ", " $
                         DL.map (\(k, v) -> k <> "=" <> v) authComponents 
            baseReq = HTTP.defaultRequest
                      { method = "POST"
                      , host = maybe "" toS (baseUrl ^? authorityL . _Just . authorityHostL . hostBSL)
                      , port = fromMaybe 80 (baseUrl ^? authorityL . _Just . authorityPortL . _Just . portNumberL)
                      , path = baseUrl ^. pathL
                      -- TODO: handle query-string
                      -- , queryString = baseUrl ^. queryL
                      , requestHeaders = [ (hContentType, "application/json")
                                         , (hAccept, "application/json")
                                         , ("X-Sentry-Auth", "Sentry " <> authString)
                                         ]
                      , checkResponse = throwErrorStatusCodes
                      }
       in Right SentryConfig
          { cfgBaseUrl = baseUrl
          , cfgKey = key
          , cfgSecret = secret
          , cfgProjectId = BS.drop 1 $ u ^. pathL
          , cfgManager = mgr
          , cfgBaseRequest = baseReq
          , cfgAuthToken = Nothing
          , cfgOrgSlug = Nothing
          }
