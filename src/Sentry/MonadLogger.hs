{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sentry.MonadLogger where

import Sentry.Types hiding (LogLevel)
import qualified Sentry.Types as Sentry (LogLevel(..))
import Sentry.Lens
import qualified Control.Monad.Logger as ML
import Control.Monad.Logger (ToLogStr, Loc, LogSource, LogLevel(..), MonadLogger)
import Control.Monad.IO.Class
import qualified Sentry.HTTP as Sentry
import GHC.Stack
import Data.Time
import Data.UUID.V4 as UUID
import Control.Monad
import Lens.Micro
import qualified Sentry.Lens as L
import Data.String.Conv
import Sentry.Blank
import Data.Maybe
import qualified System.Log.FastLogger as FL
import Sentry.SentryT
import Debug.Trace

type LoggerFn m msg = (ToLogStr msg, MonadLogger m) => Loc -> LogSource -> ML.LogLevel -> msg -> m ()


{-# INLINE defaultLogFilter #-}
defaultLogFilter :: (HasCallStack) => LogLevel -> Maybe Sentry.LogLevel
defaultLogFilter ll = case ll of
  LevelDebug -> Nothing
  LevelError -> Just Sentry.Error
  LevelWarn -> Just Sentry.Warning
  LevelInfo -> Nothing
  LevelOther t ->
    if t=="fatal"
    then Just Sentry.Fatal
    else Nothing

-- {-# INLINE sentryEnabledLogger #-}
-- sentryEnabledLogger :: (HasCallStack, ToLogStr msg, MonadLogger m, MonadIO m)
--                     => SentryConfig
--                     -> (LogLevel -> Maybe Sentry.LogLevel)
--                     -> (UTCTime -> EventId -> Loc -> LogSource -> ML.LogLevel -> msg -> m Event)
--                     -> LoggerFn m msg
--                     -> LoggerFn m msg
-- sentryEnabledLogger cfg logFilter toSentryEvent originalFn loc ls ll msg = do
--   case logFilter ll of
--     Just sl ->  void $ liftIO $ Sentry.store cfg $ toSentryEvent loc ls ll msg
--     Nothing -> pure ()
--   originalFn loc ls ll msg

defaultSetLoc :: (HasCallStack) => Loc -> Event -> Event
defaultSetLoc loc evt =
  if loc==ML.defaultLoc
  then evt
  else let baseException = fromMaybe blank $ listToMaybe $ evtException evt
           baseStacktrace = fromMaybe blank (exStacktrace baseException)
       in evt { evtException = [ baseException { exStacktrace = Just baseStacktrace { stFrames =  (locToFrame loc):(stFrames baseStacktrace) } } ] }

locToFrame :: Loc -> Frame
locToFrame l  = Frame
  { frFilename = Just $ ML.loc_filename l
  , frFunction = Nothing
  , frModule = Just $ ML.loc_module l
  , frLineno = Just $ fst $ ML.loc_start l
  , frColno = Just $ snd $ ML.loc_start l
  , frPackage = Just $ ML.loc_package l
  }

defaultSetStacktrace :: (HasCallStack) => CallStack -> Event -> Event
defaultSetStacktrace cs evt =
  let baseException = fromMaybe blank $ listToMaybe $ evtException evt
      baseStacktrace = fromMaybe blank (exStacktrace baseException)
  in evt { evtException = [ baseException { exStacktrace = Just baseStacktrace { stFrames = callStackToSentry cs } } ] }

-- defaultLogToSentryEvent :: (HasCallStack, ToLogStr msg) => Loc -> LogSource -> Sentry.LogLevel -> msg -> m Event
-- defaultLogToSentryEvent loc ls ll msg =
--   liftIO $
--   fmap (defaultSetLoc loc) $
--   fmap (defaultSetStacktrace callStack) $
--   fmap (setMessage (ML.fromLogStr $ ML.toLogStr msg)) $
--   fmap (set L.logger (Just $ toS ls)) $
--   (liftM3 mkBlank) (EventId <$> UUID.nextRandom) getCurrentTime (pure ll)

captureLog :: (HasCallStack, ToLogStr msg, HasSentry m)
             => Loc
             -> LogSource
             -> Sentry.LogLevel
             -> msg
             -> m (Maybe EventId)
captureLog loc ls ll msg = do
  traceM "captureLog / mkBlankEvent"
  evt <- mkBlankEvent
  traceM "captureLog / captureEvent"
  captureEvent $
    defaultSetLoc loc $
    defaultSetStacktrace callStack $
    setMessage (FL.fromLogStr $ ML.toLogStr msg) $
    set L.logger (Just $ toS ls) $
    set L.level ll $
    evt


-- defaultLogToSentryEvent :: (HasCallStack, ToLogStr msg)
--                         => UTCTime
--                         -> EventId
--                         -> Loc
--                         -> LogSource
--                         -> Sentry.LogLevel
--                         -> msg
--                         -> Event
-- defaultLogToSentryEvent t eid loc ls ll msg =
--   defaultSetLoc loc $
--   defaultSetStacktrace callStack $
--   setMessage (FL.fromLogStr $ ML.toLogStr msg) $
--   set L.logger (Just $ toS ls) $
--   mkBlank eid t ll

-- sentryEnabledLogger :: ToLogStr msg
--                     => Loc
--                     -> LogSource
--                     -> LogLevel
--                     -> msg
--                     -> m ()
-- sentryEnabledLogger loc ls ll msg = 

-- runStderrLoggingT :: MonadIO m
--                   => SentryConfig
--                   -> ML.LoggingT m a -> m a
-- runStderrLoggingT cfg 
