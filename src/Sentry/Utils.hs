module Sentry.Utils where

import UnliftIO
import Sentry.Blank

-- Extra options for SentryConfig
--
-- forkIO transport OR bounded channel transport OR rsyslog transport
-- sampleRate
-- beforeSend hook
-- beforeBreadcrumb hook
-- http proxy


-- scopes?
-- breadcrumbs?
-- HTTP 429 Retry-After backoff ?
-- retrieve last event ID ?
-- list of packages?

-- notify
-- notifyException
-- notifyMessage
-- notifyExceptionQ
-- notifyMessageQ

-- notifyException :: (MonadIO m, ToSentry e)
--                 => SentryConfig
--                 -> e
--                 -> m ()
-- notifyException SentryConfig{cfgEventUpdater} = do
--   let evt = 

