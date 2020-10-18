{-# LANGUAGE ExistentialQuantification #-}
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
import GHC.Stack
import Control.Exception
import Type.Reflection
import GHC.Exts (toList)

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
  , evtMessage :: !(Maybe Message)
  , evtException :: ![SentryException]
  , evtUser :: !(Maybe User)
  } deriving (Eq, Show, Generic)

instance ToJSON Event where
  toJSON Event{..} = Aeson.object $
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
    , "message" .= evtMessage
    , "user" .= evtUser
    ] <>
    (if DL.null evtException then [] else [ "exception" .= Aeson.object [ "values" Aeson..= evtException ] ])
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

data SentryException = SentryException
  { exType :: !(Maybe String)
  , exValue :: !(Maybe String)
  , exModule :: !(Maybe String)
  -- , exThreadId :: !(Maybe ThreadId)
  -- , exMechanism :: !_
  , exStacktrace :: !(Maybe SentryStacktrace)
  -- , exFrames :: ![Frame]
  -- , exRegisters :: ![(String, String)]
  } deriving (Eq, Show, Generic)

type ThreadId = String

instance ToJSON SentryException where
  toJSON = genericToJSON (Casing.aesonPrefix Casing.snakeCase)

-- data SentryThread = SentryThread
--   { thId :: !ThreadId
--   , thCrashed :: !(Maybe Boolean)
--   , thCurrent :: !(Maybe Boolean)
--   , thName :: !(Maybe String)
--   , thStacktrace :: !(Maybe SentryStacktrace)
--   }

data SentryStacktrace = SentryStacktrace
  { stFrames :: ![Frame]
  -- , stRegisters :: ![(String, String)]
  } deriving (Eq, Show, Generic)

instance ToJSON SentryStacktrace where
  toJSON = genericToJSON (Casing.aesonPrefix Casing.snakeCase)

data Frame = Frame
  { frFilename :: !(Maybe FilePath)
  , frFunction :: !(Maybe String)
  -- , frRawFunction :: !(Maybe String)
  , frModule :: !(Maybe String)
  , frLineno :: !(Maybe Int)
  , frColno :: !(Maybe Int)
  -- , frAbsPath :: !(Maybe FilePath)
  -- , frContextLine :: !(Maybe String)
  -- , frPreContext :: !(Maybe [String])
  -- , frPostContext :: !(Maybe [String])
  -- , frInApp :: !(Maybe Bool)
  -- , frVars :: ![(String, String)]
  -- , frInstructionAddr :: ()
  -- , frSymbolAddr :: ()
  -- , frImageAddr :: ()
  , frPackage :: !(Maybe String)
  -- , frPlatform :: !(Maybe Platform)
  } deriving (Eq, Show, Generic)

instance ToJSON Frame where
  toJSON = genericToJSON (Casing.aesonPrefix Casing.snakeCase)

data Message = Message
  { msgFormatted :: !(Maybe String)
  , msgTemplate :: !(Maybe (InterpolationTemplate, [InterpolationValues]))
  } deriving (Eq, Show)

type InterpolationValues = String
type InterpolationTemplate = String

instance ToJSON Message where
  toJSON Message{..} = Aeson.object $ t <> f
    where
      t = case msgTemplate of
            Nothing -> []
            Just (str, vals) -> [ "message" Aeson..= str
                                , "params" Aeson..= vals
                                ]
      f = case msgFormatted of
            Nothing -> []
            Just x -> [ "formatted" Aeson..= x ]

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

callStackToSentry :: CallStack -> [Frame]
callStackToSentry cs = DL.reverse $ (flip DL.map) (getCallStack cs) $ \(fnName, SrcLoc{..}) ->  Frame
  { frFilename = Just srcLocFile
  , frFunction = Just fnName
  , frModule = Just srcLocModule
  , frLineno = Just srcLocStartLine
  , frColno = Just srcLocStartCol
  , frPackage = Just srcLocPackage
  }

class (Exception e) => ToSentry e where
  toSentry :: e -> SentryException
  toSentry e = SentryException
    { exType = Just $ show $ typeOf e
    , exValue = Just $ displayException e
    , exModule = Nothing
    , exStacktrace = Nothing
    }

instance ToSentry SomeException
-- where
--   toSentry (SomeException e) =
--     if typeOf e == typeOf (undefined :: ExceptionWithCallStack)
--     then toSentry e
--     else SentryException { exType = Just $ show $ typeOf e
--                          , exValue = Just $ displayException e
--                          , exModule = Nothing
--                          , exStacktrace = Nothing
--                          }

data ExceptionWithCallStack = forall e . (Exception e) => ExceptionWithCallStack e CallStack
instance Exception ExceptionWithCallStack
instance Show ExceptionWithCallStack where
  show (ExceptionWithCallStack e st) = show e <> "\n" <> prettyCallStack st

instance ToSentry ExceptionWithCallStack where
  toSentry (ExceptionWithCallStack e st) = SentryException
    { exType = Just $ show $ typeOf e
    , exValue = Just $ displayException e
    , exModule = Nothing
    , exStacktrace = Just $ SentryStacktrace { stFrames = callStackToSentry st }
    }

omitNothing :: (ToJSON v, Aeson.KeyValue kv) => T.Text -> Maybe v -> [kv]
omitNothing k mv = case mv of
  Nothing -> []
  Just v -> [ k Aeson..= v ]

data User = User
  { userId :: !(Maybe String)
  , userEmail :: !(Maybe String)
  , userIpAddress :: !(Maybe String)
  , userUsername :: !(Maybe String)
  , userOtherInfo :: !(Maybe Aeson.Object)
  } deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON User{..} = Aeson.object $ core <> extra
    where
      core = omitNothing "id" userId <>
             omitNothing "email" userEmail <>
             omitNothing "ip_address" userIpAddress <>
             omitNothing "username" userUsername
      extra = case userOtherInfo of
        Nothing -> []
        Just x -> DL.map (\(k, v) -> k Aeson..= v) $
                  toList x
