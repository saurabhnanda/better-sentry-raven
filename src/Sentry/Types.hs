{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sentry.Types where

import URI.ByteString
import Sentry.Blank
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Lens.Micro
import GHC.Generics
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Char (toLower)
import qualified Data.List as DL
-- import Network.HTTP.Client as HTTP ( Manager, defaultRequest, Request, method, host, port
--                                    , path, queryString, requestHeaders, checkResponse, throwErrorStatusCodes)
import Data.Maybe
import Data.String.Conv (toS, StringConv)
import Network.HTTP.Types.Header as HTTP
import GHC.Stack
import Control.Exception
import Type.Reflection
import GHC.Exts (toList)
import qualified Network.HTTP.Client as HT
import qualified Data.CaseInsensitive as CI
import Lens.Micro.Aeson (key, _String, _Object)
import Control.Applicative ((<|>))
import UnliftIO
import qualified Network.Wai as Wai
-- import Lens.Micro

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
  , evtRequest :: !(Maybe SentryRequest)
  } deriving (Eq, Show, Generic)



setMessage :: (StringConv msg String) => msg -> Event -> Event
setMessage msg =
  let m = Message { msgFormatted = Just $ toS msg
                  , msgTemplate = Nothing
                  }
  in (\x -> x { evtMessage = Just m })

setMessageTemplate :: (InterpolationTemplate, [InterpolationValues]) -> Event -> Event
setMessageTemplate tpl =
  let m = Message { msgFormatted = Nothing
                  , msgTemplate = Just tpl
                  }
  in (\x -> x { evtMessage = Just m })


-- {-# INLINE mkMessage #-}
-- mkMessage :: (HasCallStack)
--           => EventId
--           -> UTCTime
--           -> LogLevel
--           -> String
--           -> Event
-- mkMessage eid t ll msg =

--   let m = Message { msgFormatted = Just msg
--                   , msgTemplate = Nothing
--                   }
--   in (mkMininmal eid t ll) { evtMessage = Just m }

-- mkMessageTemplate :: (HasCallStack)
--                   => EventId
--                   -> UTCTime
--                   -> LogLevel
--                   -> 
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
    , "request" .= evtRequest
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
  { exExceptionType :: !(Maybe String)
  , exValue :: !(Maybe String)
  , exModuleName :: !(Maybe String)
  -- , exThreadId :: !(Maybe ThreadId)
  -- , exMechanism :: !_
  , exStacktrace :: !(Maybe SentryStacktrace)
  -- , exFrames :: ![Frame]
  -- , exRegisters :: ![(String, String)]
  } deriving (Eq, Show, Generic, Blank)

type ThreadId = String


instance ToJSON SentryException where
  toJSON = (genericToJSON (Casing.aesonPrefix Casing.snakeCase))
    -- where
    --   overrides :: Aeson.Value -> Aeson.Value
    --   overrides v =
    --     -- let et = v at "exception_type"
    --     --     m = v ^? (key "module_name")
    --     v & at . "exception_type" .~ Nothing
    --          -- & at (key "type") ?~ et
    --          -- & at (key "module_name") .~ Nothing
    --          -- & at (key "module") ?~ m

-- instance ToJSON SentryException where
--   toJSON SentryException{..} = Aeson.object
--     [ "type" Aeson..= exExceptionType
--     , "value" Aeson..= exValue
--     , "module" Aeson..= exModuleName
--     , "stacktrace" Aeson..= exStacktrace
--     ]

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
  } deriving (Eq, Show, Generic, Blank)

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
  } deriving (Eq, Show, Generic, Blank)

instance ToJSON Frame where
  toJSON = genericToJSON (Casing.aesonPrefix Casing.snakeCase)

data Message = Message
  { msgFormatted :: !(Maybe String)
  , msgTemplate :: !(Maybe (InterpolationTemplate, [InterpolationValues]))
  } deriving (Eq, Show, Generic, Blank)

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

-- data SentryConfig = SentryConfig
--   { cfgBaseUrl :: !(URIRef Absolute)
--   , cfgKey :: !BS.ByteString
--   , cfgSecret :: !(Maybe BS.ByteString)
--   , cfgProjectId :: !BS.ByteString
--   , cfgManager :: !Manager
--   , cfgBaseRequest :: !Request
--   , cfgAuthToken :: !(Maybe BS.ByteString)
--   , cfgOrgSlug :: !(Maybe BS.ByteString)
--   -- , cfgEventUpdater :: !(Event -> Event)
--   --, cfgBeforeSend
--   --, cfgSampleRate
--   --, cfgBeforeBreadcrumb
--   --, cfgTransport / cfgClient
--   }

-- mkSentryConfig :: Manager
--                -> BS.ByteString
--                -> Either String SentryConfig
-- mkSentryConfig mgr x = case parseURI laxURIParserOptions x of
--   Left e -> Left $ show e
--   Right u ->
--     case (u ^? authorityL . _Just . authorityUserInfoL . _Just . uiUsernameL) of
--       Nothing -> Left "Secret key is mandatory"
--       Just key_ ->
--         let baseUrl = u & (authorityL . _Just . authorityUserInfoL) .~ Nothing
--                         & pathL %~ ("/api" <>)
--             secret = u ^? authorityL . _Just . authorityUserInfoL . _Just . uiPasswordL
--             authComponents = [ ("sentry_version", "7")
--                              , ("sentry_client", "better_haskell_raven/1.0")
--                              -- TODO: send the timestamp, or not?
--                              -- , ("sentry_timestamp", _)
--                              , ("sentry_key", key_)
--                              ] -- <> (maybe [] (\s -> [("sentry_secret", s)]) secret)
--             authString = BS.intercalate ", " $
--                          DL.map (\(k, v) -> k <> "=" <> v) authComponents 
--             baseReq = HTTP.defaultRequest
--                       { method = "POST"
--                       , host = maybe "" toS (baseUrl ^? authorityL . _Just . authorityHostL . hostBSL)
--                       , port = fromMaybe 80 (baseUrl ^? authorityL . _Just . authorityPortL . _Just . portNumberL)
--                       , path = baseUrl ^. pathL
--                       -- TODO: handle query-string
--                       -- , queryString = baseUrl ^. queryL
--                       , requestHeaders = [ (hContentType, "application/json")
--                                          , (hAccept, "application/json")
--                                          , ("X-Sentry-Auth", "Sentry " <> authString)
--                                          ]
--                       , checkResponse = throwErrorStatusCodes
--                       }
--        in Right SentryConfig
--           { cfgBaseUrl = baseUrl
--           , cfgKey = key_
--           , cfgSecret = secret
--           , cfgProjectId = BS.drop 1 $ u ^. pathL
--           , cfgManager = mgr
--           , cfgBaseRequest = baseReq
--           , cfgAuthToken = Nothing
--           , cfgOrgSlug = Nothing
--           }

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
    { exExceptionType = Just $ show $ typeOf e
    , exValue = Just $ displayException e
    , exModuleName = Nothing
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
  toSentry (ExceptionWithCallStack e st) =
    let fr = callStackToSentry st
    in SentryException
       { exExceptionType = Just $ show $ typeOf e
       , exValue = Just $ displayException e
       , exModuleName = (listToMaybe fr) >>= frModule
       , exStacktrace = Just $ SentryStacktrace { stFrames = fr }
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
  } deriving (Eq, Show, Generic, Blank)

instance Semigroup User where
  (<>) u1 u2 = User
    { userId = (userId u2) <|> (userId u1)
    , userEmail = (userEmail u2) <|> (userEmail u1)
    , userIpAddress = (userIpAddress u2) <|> (userIpAddress u1)
    , userUsername = (userUsername u2) <|> (userUsername u1)
    , userOtherInfo = let x = (fromMaybe mempty (userOtherInfo u1)) <> (fromMaybe mempty (userOtherInfo u2))
                      in if x==mempty
                         then Nothing
                         else Just x
    }

instance Monoid User where
  mempty = blank

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

data SentryRequest = SentryRequest
  { reqMethod :: !T.Text
  , reqIsSecure :: !Bool
  , reqPath :: !T.Text
  , reqQueryString :: !T.Text
  , reqHeaders :: ![(T.Text, T.Text)]
  , reqHost :: !(Maybe T.Text)
  , reqCookies :: !(Maybe T.Text)
  } deriving (Eq, Show)

fromWaiRequest :: Wai.Request -> SentryRequest
fromWaiRequest req = SentryRequest
  { reqMethod = toS $ Wai.requestMethod req
  , reqIsSecure = Wai.isSecure req
  , reqPath = toS $ Wai.rawPathInfo req
  , reqQueryString = toS $ Wai.rawQueryString req
  , reqHeaders = (DL.map (\(k, v) -> (toS $ CI.original k, toS v) :: (T.Text, T.Text)) $  Wai.requestHeaders req)
  , reqHost = fmap toS $ Wai.requestHeaderHost req
  , reqCookies = fmap toS $ DL.lookup hCookie (Wai.requestHeaders req)
  }

instance ToJSON SentryRequest where
  toJSON SentryRequest{..} = Aeson.object
    [  "method" Aeson..= reqMethod
    , "url" Aeson..= ((if reqIsSecure then "https://" else "http://") <>
                      (fromMaybe "unknown" reqHost) <>
                      reqPath)
    , "query_string" Aeson..= reqQueryString
    , "headers" Aeson..= reqHeaders
    , "cookies" Aeson..= reqCookies
      -- , "data" Aeson..= (toS $ Wai.strictRequestBody r)
      -- , "env" Aeson..= _
    ]



-- "_level",
-- "_name",
-- "_fingerprint",
-- "_transaction",
-- "_user",
-- "_tags",
-- "_contexts",
-- "_extras",
-- "_breadcrumbs",
-- "_event_processors",
-- "_error_processors",
-- "_should_capture",
-- "_span",
-- "_session",
-- "_force_auto_session_tracking",

data ScopeOp a = ScopeOpAdd a
               | ScopeOpReplace a
               | ScopeOpRemove
               | ScopeOpNothing
               deriving (Eq, Show, Generic)

instance Blank (ScopeOp a) where
  blank = ScopeOpNothing

class ApplyScopeOp a b where
  applyScopeOp :: a -> ScopeOp b -> a

instance ApplyScopeOp a a where
  applyScopeOp a op = case op of
    ScopeOpAdd a_ -> a_
    ScopeOpReplace a_ -> a_
    ScopeOpRemove -> a
    ScopeOpNothing -> a

instance ApplyScopeOp (Maybe a) a where
  applyScopeOp x op = case op of
    ScopeOpAdd b -> Just b
    ScopeOpReplace b -> Just b
    ScopeOpRemove -> Nothing
    ScopeOpNothing -> x

instance {-# OVERLAPS #-} ApplyScopeOp [a] [a] where
  applyScopeOp a op = case op of
    ScopeOpAdd b -> b <> a
    ScopeOpReplace b -> b
    ScopeOpRemove -> []
    ScopeOpNothing -> a

instance {-# OVERLAPS #-} ApplyScopeOp (Maybe User) User where
  applyScopeOp u1 op = case op of
    ScopeOpNothing -> u1
    ScopeOpRemove -> Nothing
    ScopeOpReplace u2 -> Just u2
    ScopeOpAdd u2 -> Just $ (fromMaybe mempty u1) <> u2

data Scope = Scope
  { scopeLevel :: !(ScopeOp LogLevel)
  , scopeFingerprint :: !(ScopeOp [String])
  , scopeUser :: !(ScopeOp User)
  , scopeTags :: !(ScopeOp [(String, String)])
  , scopeExtra :: !(ScopeOp [(String, String)])
  , scopeTransaction :: !(ScopeOp String)
  , scopeLastEventId :: !(Maybe EventId)
  -- , scopeBreadcrumbs :: ![Breadcrumb]
  -- TODO
  --, scopeContexts ::
  } deriving (Eq, Show, Generic, Blank)

data Breadcrumb = Breadcrumb {}


data SentryService = SentryService
  { svcDsn :: !(URIRef Absolute)
  , svcKey :: !BS.ByteString
  , svcSecret :: !(Maybe BS.ByteString)
  , svcProjectId :: !BS.ByteString
  , svcTransport :: (Event -> IO ())
  , svcDisabled :: !Bool
  , svcSampleRate :: !Float
  , svcBeforeSend :: (Event -> IO (Maybe Event))
  , svcScopeRef :: !(IORef Scope)
  , svcEventDefaults :: !(Event -> Event)
  }

stderrTransport :: Event
                -> IO (Maybe EventId)
stderrTransport evt = do
  BSL.hPutStr stderr $ (Aeson.encode evt) <> "\n"
  pure Nothing

mkSentryService :: HasCallStack
                => BS.ByteString
                -> (Event -> Event)
                -> (SentryService -> Event -> IO ())
                -> IO SentryService
mkSentryService dsn applyDefaults transport = case parseURI laxURIParserOptions dsn of
  Left e -> error $ show e
  Right dsnUri -> do
    scopeRef <- newIORef blank
    let svc = SentryService
              { svcDsn = dsnUri
              , svcKey = fromMaybe (error "Secret key is mandatory") $
                         dsnUri ^? authorityL . _Just . authorityUserInfoL . _Just . uiUsernameL
              , svcSecret = dsnUri ^? authorityL . _Just . authorityUserInfoL . _Just . uiPasswordL
              , svcProjectId = BS.drop 1 $ dsnUri ^. pathL
              , svcDisabled = False
              , svcSampleRate = 1.0
              , svcBeforeSend = pure . Just
              , svcTransport = transport svc
              , svcScopeRef = scopeRef
              , svcEventDefaults = applyDefaults
              }
    pure svc

-- setTransport :: (Event -> IO (Maybe EventId))
--              -> SentryService
--              -> SentryService
-- setTransport t s = s { svcTransport = t }

-- data SentrySdk m = HasSentry m => SentrySdk
--   { sdkTransport :: !(Event -> m (Maybe EventId))
--   , sdkDisabled :: !Bool
--   , sdkSampleRate :: !Float
--   , sdkBeforeSend :: !(Event -> m Event)
--   , sdkScope :: !Scope
--   -- TODO: , sdkBeforeBreadcrumb :: !Breadcrumb
--   }
