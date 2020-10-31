{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sentry.SentryT where

import Sentry.Types
import System.Random (randomRIO)
import qualified Sentry.Lens as L
import Lens.Micro
import Control.Applicative ((<|>))
import Data.Maybe
import Data.Bool
import qualified Data.List as DL
import UnliftIO
import Control.Monad.Reader
import Sentry.Blank
import Data.String.Conv
import Debug.Trace
import GHC.Stack
import Data.UUID.V4 as UUID
import Data.Time

type SentryT = ReaderT SentryService

class (MonadIO m) => HasSentry m where
  getSentryService :: m SentryService
  clearSentryScope :: m ()

instance (MonadIO m) => HasSentry (SentryT m) where
  {-# INLINE getSentryService #-}
  getSentryService = ask

  clearSentryScope = do
    SentryService{svcScopeRef} <- getSentryService
    writeIORef svcScopeRef blank

getLastEventId :: (HasSentry m) => m (Maybe EventId)
getLastEventId = do
  SentryService{svcScopeRef} <- getSentryService
  Scope{scopeLastEventId} <- readIORef svcScopeRef
  pure scopeLastEventId

-- Scope
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


-- captureEvent
--   1. If the SDK is disabled, Sentry discards the event right away.
--   2. The client samples events as defined by the configured sample
--      rate. Events may be discarded randomly, according to the sample rate.
--   3. The scope is applied, using apply_to_event. The scope’s
--      event processors are invoked in order.
--   4. Sentry invokes the before-send hook.
--   5. Sentry passes the event to the configured transport. The transport
--      can discard the event if it does not have a valid DSN; its internal
--      queue is full; or due to rate limiting, as requested by the server.
--
captureEvent :: (HasSentry m) => Event -> m ()
captureEvent evt = do
  SentryService{svcTransport, svcDisabled, svcSampleRate, svcBeforeSend, svcScopeRef, svcEventDefaults} <- getSentryService
  case svcDisabled of
    True -> pure ()
    False -> liftIO $ do
      r :: Float <- randomRIO (0.0, 1.0)

      -- TODO: Debug logging in the case when event is discared?
      case (r < svcSampleRate) of
        False -> pure ()
        True -> do
          scope <- readIORef svcScopeRef
          svcBeforeSend (applyToEvent scope $ svcEventDefaults evt) >>= \case
            Just finalEvent -> svcTransport finalEvent
            Nothing -> pure ()

captureMessage :: (HasSentry m, StringConv msg String)
               => msg
               -> LogLevel
               -> m ()
captureMessage msg ll = do
  evt <- mkBlankEvent
  captureEvent $ setMessage msg evt { evtLevel = ll }

-- captureException
-- captureException :: (HasSentry m, ToSentry e) => e -> m ()
-- captureException e = captureEvent

-- captureMessage
-- addBreadcrumb

-- configureScope :: (HasSentry m1, MoonadIO m2)
--                => (Scope -> m2 Scope)
--                -> m1 ()
-- configureScope fn = do
--   SentryService{cfgDisabled, cfgScopeMVar} <- getSentryService
--   when (not cfgDisabled) $ do
--     modifyMVar_ cfgScopeMVar (liftIO . fn)


applyToEvent :: Scope -> Event -> Event
applyToEvent Scope{..} evt@Event{..} = evt
  { evtLevel = applyScopeOp evtLevel scopeLevel
  , evtUser = applyScopeOp evtUser scopeUser
  , evtTags = applyScopeOp evtTags scopeTags
  , evtExtra = applyScopeOp evtExtra scopeExtra
  , evtTransaction = applyScopeOp evtTransaction scopeTransaction
  , evtFingerprint = applyScopeOp evtFingerprint scopeFingerprint
  -- , evtBreadcrumbs = evtBreadcrumbs <> scopeBreadcrumbs
  }

setLevel :: LogLevel -> Scope -> Scope
setLevel x s = s { scopeLevel = ScopeOpReplace x }

setFingerprint :: [String] -> Scope -> Scope
setFingerprint x s = s { scopeFingerprint = ScopeOpReplace x }

setUser :: User -> Scope -> Scope
setUser x s = s { scopeUser = ScopeOpReplace x }

mergeUser :: User -> Scope -> Scope
mergeUser x s = s { scopeUser = ScopeOpAdd x }

setTags :: [(String, String)] -> Scope -> Scope
setTags x s = s { scopeTags = ScopeOpReplace x }

setExtra :: [(String, String)] -> Scope -> Scope
setExtra x s = s { scopeExtra = ScopeOpReplace x }

mergeTags :: [(String, String)] -> Scope -> Scope
mergeTags x s = s { scopeTags = ScopeOpAdd x }

mergeExtra :: [(String, String)] -> Scope -> Scope
mergeExtra x s = s { scopeExtra = ScopeOpAdd x }

setTransaction :: String -> Scope -> Scope
setTransaction x s = s { scopeTransaction = ScopeOpReplace x }


{-# INLINE mkBlankEvent #-}
mkBlankEvent :: (HasCallStack, MonadIO m)
             => m Event
mkBlankEvent = do
  (eid, t) <- liftIO $ (,) <$> UUID.nextRandom <*> getCurrentTime
  -- applyDefaults <- svcEventDefaults <$> getSentryService
  pure Event
    { evtId = EventId eid
    , evtTimestamp = t
    , evtPlatform = PlatformHaskell
    , evtLevel = Info
    , evtLogger = Nothing
    , evtTransaction = Nothing
    , evtServerName = Nothing
    , evtRelease = Nothing
    , evtDist = Nothing
    , evtTags = []
    , evtEnvironment = Nothing
    , evtModules = []
    , evtExtra = []
    , evtFingerprint = []
    , evtMessage = Nothing
    , evtException = []
    , evtUser = Nothing
    , evtRequest = Nothing
    }
