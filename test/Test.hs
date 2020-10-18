{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test where

import Test.Tasty as Tasty
import Test.Tasty.Hedgehog as Tasty
import Test.Tasty.HUnit as Tasty
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Gen.Generic
import Sentry.Types
import Data.UUID as UUID
import Data.Time.Clock.POSIX
import Data.Time
import Data.Either
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Sentry.HTTP as Sentry
import UnliftIO
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Debug.Trace
import GHC.Stack
import Lens.Micro
import Lens.Micro.Aeson
import Data.Maybe
import qualified Control.Retry as Retry
import qualified Control.Monad.Catch as Catch
import Hedgehog.Internal.Property (writeLog, Log (Annotation))
import Hedgehog.Internal.Source (getCaller)
import Hedgehog.Internal.Show (showPretty)
-- import Data.Function ((&))
-- import Data.UUID.V4


data TestEnv = TestEnv
  { envCfg :: !SentryConfig
  , envTime :: !UTCTime
  }

main :: IO ()
main = do
  mgr <- getGlobalManager
  envTime <- getCurrentTime
  let envCfg = either error (\x -> x{cfgAuthToken=tkn, cfgOrgSlug=slug}) $
            mkSentryConfig mgr "http://edd7aa040752461e9f434724deb3dd03@168.119.172.134:9000/1"
      tkn = Just "19aece92cc484af5aa9f5c7c09128dca3ca87123070f4789bcd2b3e3f6e53f48"
      slug = Just "sentry"
      env = TestEnv{..}
  defaultMain $
    testGroup "All tests"
    [ testProperty "Basic events" $ propBasicEvent env
    , testCase "List organizations" $ testListAllOrganizations env
    , testCase "Resolve event Id" $ testResolveEventId env
    ]

-- propBasicEvent :: Property
-- propBasicEvent = _todo

genBasicEvent :: UTCTime -> Gen Event
genBasicEvent curTime = do
  evtId <- genEventId
  evtTimestamp <- genUTCTime curTime
  evtPlatform <- genPlatform
  evtLevel <- Gen.enumBounded
  let maybeString = Gen.maybe $ Gen.string (Range.linear 1 1000) Gen.alphaNum
      t = (,) <$> (Gen.string (Range.linear 1 100) Gen.alphaNum) <*> (Gen.string (Range.linear 1 100) Gen.alphaNum)
  evtLogger <- maybeString
  evtTransaction <- maybeString
  evtRelease <- maybeString
  evtDist <- maybeString
  evtEnvironment <- maybeString
  evtFingerprint <- Gen.list (Range.linear 1 10) $ Gen.string (Range.linear 1 1000) Gen.alphaNum
  evtServerName <- maybeString
  evtTags <- Gen.list (Range.linear 1 10) t
  evtModules <- Gen.list (Range.linear 1 10) t
  evtExtra <- Gen.list (Range.linear 1 10) t
  pure Event{..}

genUUID :: Gen UUID
genUUID =
  let x = Gen.word32 (Range.linear 0 1000)
  in UUID.fromWords <$> x <*> x <*> x <*> x

genEventId :: Gen EventId
genEventId = EventId <$> genUUID

genPlatform :: Gen Platform
genPlatform = pure PlatformHaskell

genUTCTime :: UTCTime -> Gen UTCTime
genUTCTime t = do
  d <- (Gen.integral (Range.linear (-1*60*60*24) (1*60*60*24)))
  pure $ addUTCTime (fromInteger d) t

-- genBasicEvent :: Gen Event
-- genBasicEvent = -- mkGenWith (genUUID :& GNil) $ byType genUUID emptyGens
--   mkGenWith (genEventId :& GNil) emptyGens

assertEitherM :: (HasCallStack, Show e)
              => String
              -> IO (Either e r)
              -> IO r
assertEitherM msg action = do
  action >>= \case
    Left e -> assertFailure $ msg <> ": Unexpected Left value: " <> show e
    Right r -> pure r

{-# INLINE tapAnnotateShow #-}
tapAnnotateShow :: (MonadTest m, Show a, HasCallStack) => a -> m a
tapAnnotateShow a = (writeLog $ Annotation (getCaller callStack) (showPretty a)) >> (pure a)

{-# INLINE tapAnnotateShowM #-}
tapAnnotateShowM :: (MonadTest m, Show a, HasCallStack) => m a -> m a
tapAnnotateShowM action = do
  a <- action
  (writeLog $ Annotation (getCaller callStack) (showPretty a))
  pure a

isProblematicEvent :: Event -> Bool
isProblematicEvent Event{..} =
  (evtLogger == Nothing) &&
  (evtTransaction == Nothing) &&
  (evtServerName == Nothing) &&
  (evtRelease == Nothing) &&
  (evtDist == Nothing) &&
  (evtTags == []) &&
  (evtExtra == []) &&
  (evtFingerprint == []) &&
  (evtModules == [])

propBasicEvent :: (HasCallStack) => TestEnv -> Property
propBasicEvent TestEnv{..} = property $ do
  evt <- forAll $ genBasicEvent envTime
  r <- (evalIO $ Sentry.store envCfg evt) >>= tapAnnotateShow
  case r ^? _Right . (key "id") . _String of
    Nothing -> failure
    Just _ -> success

  -- case isProblematicEvent evt of
  --   True -> do
  --     traceM "discarded"
  --     discard
  --   False -> do
  --     r <- (evalIO $ Sentry.store envCfg evt) >>= tapAnnotateShow
  --     case r ^? _Right . (key "id") . _String of
  --       Nothing -> failure
  --       Just _ -> do
  --         traceM "pass"
  --         success
          -- r2 <- (evalIO $ retryOnTemporaryNetworkErrors (ignoreStatus [404, 429]) $ Sentry.resolveEventId envCfg (evtId evt))
          --       >>= tapAnnotateShow
          -- case r2 ^? _Right . (key "event") of
          --   Nothing -> failure
          --   Just evt2 -> case evt2 ^? (key "errors") . _Array of
          --     Nothing -> success
          --     Just x -> do
          --       mempty === x
          --       traceM "pass"

testListAllOrganizations :: TestEnv -> IO ()
testListAllOrganizations TestEnv{..} = do
  void $ assertEitherM "Expecting a valid JSON" $ Sentry.listAllOrganizations envCfg

testResolveEventId :: TestEnv -> IO ()
testResolveEventId TestEnv{..} = do
  evt <- Gen.sample $ genBasicEvent envTime
  resp <- Sentry.store envCfg evt
  case resp ^? _Right . (key "id") . _String of
    Nothing -> assertFailure "Expecting an event id in response"
    Just eid -> void $ assertEitherM "Unexpected nonsense" $
                retryOnTemporaryNetworkErrors (ignoreStatus [404, 429]) $
                Sentry.resolveEventId envCfg (httpToEventId eid)

ignoreStatus :: [Int] -> Status -> Bool
ignoreStatus xs st = statusCode st `elem` xs

retryOnTemporaryNetworkErrors :: (MonadIO m, Catch.MonadMask m)
                              => (Status -> Bool)
                              -> m a
                              -> m a
retryOnTemporaryNetworkErrors statusCodeHandler action =
  Retry.recovering policy [const (Catch.Handler httpExHandler)] (const action)
  where
    policy = (Retry.constantDelay 1000000) <> (Retry.limitRetries 10)
    httpExHandler (e :: HttpException) = case e of
      InvalidUrlException _ _ -> pure False
      HttpExceptionRequest _ e2 -> case e2 of
        StatusCodeException resp _ -> pure $ statusCodeHandler $ responseStatus resp
        TooManyRedirects _ -> pure False
        OverlongHeaders -> pure False
        ResponseTimeout -> pure True
        ConnectionTimeout -> pure True
        ConnectionFailure _ -> pure True
        InvalidStatusLine _ -> pure False
        InvalidHeader _ -> pure False
        InvalidRequestHeader _ -> pure False
        InternalException _ -> pure False
        ProxyConnectException _ _ _ -> pure True
        NoResponseDataReceived -> pure False
        TlsNotSupported -> pure False
        WrongRequestBodyStreamSize _ _ -> pure False
        ResponseBodyTooShort _ _ -> pure False
        InvalidChunkHeaders -> pure False
        IncompleteHeaders -> pure False
        InvalidDestinationHost _ -> pure False
        HttpZlibException _ -> pure False
        InvalidProxyEnvironmentVariable _ _ -> pure False
        ConnectionClosed -> pure True
        InvalidProxySettings _ -> pure False
