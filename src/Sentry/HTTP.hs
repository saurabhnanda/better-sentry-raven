{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sentry.HTTP where

import Network.HTTP.Types
import Network.HTTP.Client as HTTP
import Sentry.Types
import Data.Aeson as Aeson
import Debug.Trace
import GHC.Stack
import URI.ByteString
import qualified Data.ByteString as BS
import Safe (fromJustNote)
import Network.HTTP.Client.TLS (getGlobalManager)
import URI.ByteString
import Lens.Micro
import Lens.Micro.Aeson (key, _String)
import UnliftIO
import qualified Data.List as DL
import Data.Maybe
import Data.String.Conv
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.String.Conv (toS)

-- data HttpConfig = HttpConfig
--   { cfgManager :: !Manager
--   , cfgBaseRequest :: !Request
--   }

-- mkHttpConfig :: Maybe Manager
--              -> (HttpConfig -> HttpConfig)
--              -> HttpConfig
-- mkHttpConfig mManager overrideFn = do
--   cfgManager <- fromMaybe getGlobalManager mManager
--   -- let cfgBaseRequest =
--   pure $ overrideFn HttpConfig{..}


mkHttpTransport :: (HasCallStack, Exception e)
                => Bool
                -> Manager
                -> (Event -> e -> IO ())
                -> SentryService
                -> IO (Event -> IO ())
mkHttpTransport asyncFlag mgr fallbackTransport SentryService{..} = do
  let authComponents = [ ("sentry_version", "7")
                       , ("sentry_client", "better_haskell_raven/1.0")
                       -- TODO: send the timestamp, or not?
                       -- , ("sentry_timestamp", _)
                       , ("sentry_key", svcKey)
                       ] -- <> (maybe [] (\s -> [("sentry_secret", s)]) secret)
      authString = BS.intercalate ", " $
                   DL.map (\(k, v) -> k <> "=" <> v) authComponents
      -- secret = cfgDsn ^? authorityL . _Just . authorityUserInfoL . _Just . uiPasswordL

  r <- HTTP.parseRequest $ toS svcDsn
  let baseReq = r { path = "/api/" <> svcProjectId <> "/store/"
                  , requestHeaders = [ (hContentType, "application/json")
                                     , (hAccept, "application/json")
                                     , ("X-Sentry-Auth", "Sentry " <> authString)
                                     ]
                  , checkResponse = throwErrorStatusCodes
                  }
      -- TODO: Handle rate-limiting?
      transport evt = (flip catch) (fallbackTransport evt) $ do
        let req = baseReq { requestBody = RequestBodyLBS (Aeson.encode evt) }
        (fmap (Aeson.eitherDecode . responseBody) $ httpLbs req mgr) >>= \case
          Left e -> throwString e
          Right (r :: Aeson.Value) -> pure ()
  case asyncFlag of
    True -> pure $ void . forkIO . transport
    False -> pure transport

-- store :: (HasCallStack)
--       => SentryConfig
--       -> Event
--       -> IO (Either String Aeson.Value)
-- store SentryConfig{..} evt = do
--   let finalReq = cfgBaseRequest { requestBody = RequestBodyLBS (Aeson.encode evt)
--                                 , path = (path cfgBaseRequest) <> "/store/"
--                                 }
--   (httpLbs finalReq cfgManager) >>= (pure . Aeson.eitherDecode . responseBody)

-- apiAuthHeaders :: Maybe BS.ByteString
--                -> RequestHeaders
-- apiAuthHeaders tkn = [ (hContentType, "application/json")
--                      , (hAccept, "application/json")
--                      , (hAuthorization, maybe "" ("Bearer " <>) tkn)
--                      ]

-- listAllOrganizations :: (HasCallStack)
--                      => SentryConfig
--                      -> IO (Either String Aeson.Value)
-- listAllOrganizations SentryConfig{..} = do
--   let req = cfgBaseRequest { path = "/api/0/organizations/"
--                            , method = "GET"
--                            , requestHeaders = apiAuthHeaders cfgAuthToken
--                            }
--   (httpLbs req cfgManager) >>= (pure . Aeson.eitherDecode . responseBody)

-- resolveEventId :: (HasCallStack)
--                => SentryConfig
--                -> EventId
--                -> IO (Either String Aeson.Value)
-- resolveEventId SentryConfig{..} evtId =
--   let req = cfgBaseRequest { path = "/api/0/organizations/" <>
--                                     (fromJustNote "org-slug is missing" cfgOrgSlug) <>
--                                     "/eventids/" <> eventIdToHttp evtId <> "/"
--                            , method = "GET"
--                            , requestHeaders = apiAuthHeaders cfgAuthToken
--                            }
--   in (httpLbs req cfgManager) >>= (pure . Aeson.eitherDecode . responseBody)
