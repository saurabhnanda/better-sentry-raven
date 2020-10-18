{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sentry.HTTP where

import Network.HTTP.Types
import Network.HTTP.Client
import Sentry.Types
import Data.Aeson as Aeson
import Debug.Trace
import GHC.Stack
import URI.ByteString
import qualified Data.ByteString as BS
import Safe (fromJustNote)

store :: (HasCallStack)
      => SentryConfig
      -> Event
      -> IO (Either String Aeson.Value)
store SentryConfig{..} evt = do
  let finalReq = cfgBaseRequest { requestBody = RequestBodyLBS (Aeson.encode evt)
                                , path = (path cfgBaseRequest) <> "/store/"
                                }
  (httpLbs finalReq cfgManager) >>= (pure . Aeson.eitherDecode . responseBody)

apiAuthHeaders :: Maybe BS.ByteString
               -> RequestHeaders
apiAuthHeaders tkn = [ (hContentType, "application/json")
                     , (hAccept, "application/json")
                     , (hAuthorization, maybe "" ("Bearer " <>) tkn)
                     ]

listAllOrganizations :: (HasCallStack)
                     => SentryConfig
                     -> IO (Either String Aeson.Value)
listAllOrganizations SentryConfig{..} = do
  let req = cfgBaseRequest { path = "/api/0/organizations/"
                           , method = "GET"
                           , requestHeaders = apiAuthHeaders cfgAuthToken
                           }
  (httpLbs req cfgManager) >>= (pure . Aeson.eitherDecode . responseBody)

resolveEventId :: (HasCallStack)
               => SentryConfig
               -> EventId
               -> IO (Either String Aeson.Value)
resolveEventId SentryConfig{..} evtId =
  let req = cfgBaseRequest { path = "/api/0/organizations/" <>
                                    (fromJustNote "org-slug is missing" cfgOrgSlug) <>
                                    "/eventids/" <> eventIdToHttp evtId <> "/"
                           , method = "GET"
                           , requestHeaders = apiAuthHeaders cfgAuthToken
                           }
  in (httpLbs req cfgManager) >>= (pure . Aeson.eitherDecode . responseBody)
