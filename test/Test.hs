{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test where

import qualified Data.List as DL
import Test.Tasty as Tasty
import Test.Tasty.Hedgehog as Tasty
import Test.Tasty.HUnit as Tasty
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Gen.Generic
import Sentry.Types as Sentry
import qualified Sentry.Blank as Sentry
import Data.UUID as UUID
import Data.Time.Clock.POSIX
import Data.Time
import Data.Either
import Network.HTTP.Client.TLS
import Network.HTTP.Client as HT
import Network.HTTP.Types as HT
import qualified Sentry.HTTP as Sentry
import qualified Sentry.SentryT as Sentry
import UnliftIO
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Debug.Trace
import GHC.Stack
import Lens.Micro
import Lens.Micro.Aeson
import Data.Aeson as Aeson
import Data.Maybe
import qualified Control.Retry as Retry
import qualified Control.Monad.Catch as Catch
import Hedgehog.Internal.Property (writeLog, Log (Annotation))
import Hedgehog.Internal.Source (getCaller)
import Hedgehog.Internal.Show (showPretty)
import Control.Exception
import Type.Reflection
import GHC.Word
import Data.String.Conv
import qualified Data.CaseInsensitive as CI
import Control.Monad.Reader
import qualified Network.Wai as Wai
-- import Data.Function ((&))
-- import Data.UUID.V4


data TestEnv = TestEnv
  { envService :: !SentryService
  , envTime :: !UTCTime
  }

fallbackTransport :: Event -> SomeException -> IO ()
fallbackTransport evt e = do
  putStrLn $ "Error: " <> show e <> "\n"
  putStrLn $ "Payload: " <> (toS $ Aeson.encode evt)

main :: IO ()
main = do
  mgr <- getGlobalManager
  envTime <- getCurrentTime
  envService <- Sentry.mkSentryService "http://edd7aa040752461e9f434724deb3dd03@168.119.172.134:9000/1" Prelude.id $
                Sentry.mkHttpTransport True mgr fallbackTransport
  let env = TestEnv{..}
  defaultMain $
    testGroup "All tests"
    [ testCase "Event with stacktrace" $ testWithExceptionStacktrace env
    , testCase "Event with user" $ testWithUser env
    , testCase "Event with wai request" $ testWithWaiRequest env
    , testProperty "beforeSend" $ propApplyDefaults env
    , testProperty "applyScope" $ propScope env
    , testProperty "configureScope" $ propConfigureScope env
    -- , testProperty "Basic events" $ propBasicEvent env
    ]

-- propBasicEvent :: Property
-- propBasicEvent = _todo

genEmail :: Gen String
genEmail = do
  x <- Gen.string (Range.linear 1 100) Gen.alphaNum
  y <- Gen.string (Range.linear 1 100) Gen.alphaNum
  z <- Gen.string (Range.linear 1 5) Gen.alphaNum
  pure $ x <> "@" <> y <> "." <> z

genIpAddress :: Gen String
genIpAddress = do
  xs <- Gen.list (Range.constant 4 4) $
        Gen.integral (Range.linear 0 255)
  pure $
    DL.intercalate "." $
    DL.map show xs

genUser :: Gen User
genUser = do
  userId <- Gen.maybe $ Gen.string (Range.linear 1 100) Gen.alphaNum
  userEmail <- Gen.maybe genEmail
  userIpAddress <- Gen.maybe genIpAddress
  userUsername <- Gen.maybe $ Gen.string (Range.linear 1 100) Gen.alphaNum
  let userOtherInfo = Nothing
  pure User{..}

genKvp :: Int -> Int -> Gen (String, String)
genKvp l1 l2 = (,)
  <$> (Gen.string (Range.linear 1 l1) Gen.alphaNum)
  <*> (Gen.string (Range.linear 1 l2) Gen.alphaNum)

genKvpAssignments :: Int -> Int -> Gen [String]
genKvpAssignments l1 l2 =
  fmap (DL.map (\(k, v) -> k <> "=" <> v)) $
  Gen.list (Range.linear 0 10) $
  genKvp l1 l2



genFrame :: (HasCallStack) => Gen Frame
genFrame = do
  frFilename <- Gen.maybe $
                fmap (DL.intercalate "/") $
                Gen.list (Range.linear 1 5) $
                Gen.string (Range.linear 1 40) Gen.alphaNum
  frFunction <- Gen.maybe $
                Gen.string (Range.linear 1 40) Gen.alphaNum
  frModule <- Gen.maybe $
              Gen.string (Range.linear 1 40) Gen.alphaNum
  frLineno <- Gen.maybe $
              Gen.integral (Range.linear 1 1000)
  frColno <- Gen.maybe $
             Gen.integral (Range.linear 1 1000)
  frPackage <- Gen.maybe $
               Gen.string (Range.linear 1 40) Gen.alphaNum
  pure Frame{..}

genSentryStacktrace :: (HasCallStack) => Gen SentryStacktrace
genSentryStacktrace = do
  stFrames <- Gen.list (Range.linear 1 50) genFrame
  -- stRegisters <- Gen.list (Range.linear 1 16) (genKvp 100 100)
  pure SentryStacktrace{..}

genSentryException :: (HasCallStack) => Gen SentryException
genSentryException = do
  exExceptionType <- Gen.maybe $ fmap ("TYPE: " <>) $ Gen.string (Range.linear 1 100) Gen.alphaNum
  exValue <- Gen.maybe $ fmap ("VALUE: " <>) $ Gen.string (Range.linear 1 100) Gen.alphaNum
  exModuleName <- Gen.maybe $ fmap ("MOD: " <> ) $ Gen.string (Range.linear 1 100) Gen.alphaNum
  exStacktrace <- Gen.maybe genSentryStacktrace
  pure SentryException{..}

genMsgTemplate :: (HasCallStack) => Gen (InterpolationTemplate, [InterpolationValues])
genMsgTemplate = do
  i <- Gen.integral (Range.linear 1 5)
  str <- fmap (DL.intercalate " %s ") $
         Gen.list (Range.constant (i+1) (i+1)) $
         Gen.list (Range.linear 10 20) Gen.alphaNum
  vals <- Gen.list (Range.constant i i) $
          Gen.list (Range.linear 1 10) Gen.alphaNum
  pure (str, vals)

genMessage :: (HasCallStack) => Gen Message
genMessage = do
  msgTemplate <- Gen.maybe genMsgTemplate
  msgFormatted <- Gen.maybe $ Gen.list (Range.linear 1 100) Gen.alphaNum
  pure Message{..}

genBasicEvent :: (HasCallStack) => UTCTime -> Gen Event
genBasicEvent curTime = do
  evtId <- genEventId curTime
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
  evtMessage <- Gen.maybe genMessage
  let evtException = []
      evtUser = Nothing
      evtRequest = Nothing
      evtThreads = []
  pure Event{..}

genEventWithException :: (HasCallStack) => UTCTime -> Gen Event
genEventWithException curTime = do
  evt <- genBasicEvent curTime
  ex <- Gen.list (Range.linear 0 5) genSentryException
  pure evt { evtException = ex }

genUUID :: UTCTime -> Gen UUID
genUUID t =
  let x = Gen.word32 (Range.linear (fromIntegral $ round $ utcTimeToPOSIXSeconds t) (maxBound :: Word32))
  in UUID.fromWords <$> x <*> x <*> x <*> x

genEventId :: UTCTime -> Gen EventId
genEventId t = EventId <$> genUUID t

genPlatform :: Gen Platform
genPlatform = pure PlatformHaskell

genUTCTime :: UTCTime -> Gen UTCTime
genUTCTime t = do
  d <- (Gen.integral (Range.linear (-1*60*60*24) 0))
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

propScope :: (HasCallStack) => TestEnv -> Property
propScope TestEnv{..} = property $ do
  evt <- forAll $ genBasicEvent envTime
  scope <- forAll $ genScope False
  evt === (Sentry.applyToEvent Sentry.blank evt)
  if scope /= Sentry.blank then (Sentry.applyToEvent scope evt) /== evt else pure ()
  (Sentry.applyToEvent scope (Sentry.applyToEvent scope evt)) === (Sentry.applyToEvent scope evt)

propApplyDefaults :: (HasCallStack) => TestEnv -> Property
propApplyDefaults TestEnv{..} = property $ do
  (randEvt, overrideEvt) <- forAll $ do
    (,) <$> genBasicEvent envTime <*> genBasicEvent envTime
  ref <- newIORef []
  svc <- liftIO $ Sentry.mkSentryService "http://edd7aa040752461e9f434724deb3dd03@168.119.172.134:9000/1" (const overrideEvt) $
         (\_ evt -> void $ modifyIORef' ref (\x -> evt:x))
  (flip runReaderT) svc $ do
    void $ Sentry.captureEvent randEvt
  readIORef ref >>= tapAnnotateShow >>= \case
    [] -> annotate "No events captured" >> failure
    [evt] -> evt === overrideEvt
    _ -> annotate "Multipe events captured. Expecting only a single event" >> failure

genScopeOp :: Bool -> Gen a -> Gen (ScopeOp a)
genScopeOp addFlag g =
  Gen.choice $
  [ pure ScopeOpNothing
  , pure ScopeOpRemove
  , ScopeOpReplace <$> g
  ] <> (if addFlag then [ScopeOpAdd <$> g] else [])

genScope :: Bool -> Gen Scope
genScope addFlag = Scope
  <$> (g Gen.enumBounded)
  <*> (g $ Gen.list (Range.linear 1 5) $ Gen.string (Range.linear 1 30) Gen.alphaNum)
  <*> (g genUser)
  <*> (g $ Gen.list (Range.linear 1 5) $ genKvp 5 30)
  <*> (g $ Gen.list (Range.linear 1 5) $ genKvp 5 30)
  <*> (g $ Gen.string (Range.linear 1 30) Gen.alphaNum)
  <*> (pure Nothing)
  where
    g :: Gen a -> Gen (ScopeOp a)
    g = genScopeOp addFlag
-- propApplyScope :: (HasCallStack) => TestEnv -> Property
-- propApplyScope TestEnv{..} = property $ do
--   (randEvt, overrideEvt) <- forAll $ do
--     (,) <$> genBasicEvent envTime <*> genBasicEvent envTime
--   ref <- newIORef []
--   svc <- liftIO $ Sentry.mkSentryService "http://edd7aa040752461e9f434724deb3dd03@168.119.172.134:9000/1" (const overrideEvt) $
--          (\_ evt -> modifyIORef' ref (\x -> evt:x) >> pure Nothing)
--   (flip runReaderT) svc $ do
--     void $ Sentry.captureEvent randEvt
--   readIORef ref >>= tapAnnotateShow >>= \case
--     [] -> annotate "No events captured" >> failure
--     [evt] -> evt === overrideEvt
--     _ -> annotate "Multipe events captured. Expecting only a single event" >> failure


-- propBasicEvent :: (HasCallStack) => TestEnv -> Property
-- propBasicEvent TestEnv{..} = property $ do
--   evt <- forAll $ genBasicEvent envTime
--   r <- (evalIO $ Sentry.captureEvent envCfg evt) >>= tapAnnotateShow
--   case r ^? _Right . (key "id") . _String of
--     Nothing -> failure
--     Just _ -> success

  -- case isProblematicEvent evt of
  --   True -> do
  --     traceM "discarded"
  --     discard
  --   False -> do
  --     r <- (evalIO $ Sentry.captureEvent envCfg evt) >>= tapAnnotateShow
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

-- testListAllOrganizations :: TestEnv -> IO ()
-- testListAllOrganizations TestEnv{..} = do
--   void $ assertEitherM "Expecting a valid JSON" $ Sentry.listAllOrganizations envCfg

-- testResolveEventId :: TestEnv -> IO ()
-- testResolveEventId TestEnv{..} = do
--   evt <- Gen.sample $ genBasicEvent envTime
--   resp <- Sentry.captureEvent envCfg evt
--   case resp ^? _Right . (key "id") . _String of
--     Nothing -> assertFailure "Expecting an event id in response"
--     Just eid -> void $ assertEitherM "Unexpected nonsense" $
--                 retryOnTemporaryNetworkErrors (ignoreStatus [404, 429]) $
--                 Sentry.resolveEventId envCfg (httpToEventId eid)

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


genWaiRequest :: Gen Wai.Request
genWaiRequest = do
  method_ <- Gen.element ["GET", "POST", "PUT", "HEAD", "DELETE"]
  host_ <- Gen.string (Range.linear 1 100) Gen.alphaNum
  port_ <- Gen.integral (Range.linear 1 65535)
  path_ <- fmap (DL.intercalate "/") $
           Gen.list (Range.linear 1 5) $
           Gen.string (Range.linear 1 10) Gen.alphaNum
  queryString_ <- fmap (DL.intercalate "&") $ genKvpAssignments 10 50
  mCookieHeader <- Gen.maybe $ (,)
                  <$> (pure hCookie)
                  <*> (fmap toS $ fmap (DL.intercalate "; ") $ genKvpAssignments 10 50)
  mHostHeader <- Gen.maybe $ (,)
                 <$> (pure $ "Host")
                 <*> (fmap toS $ Gen.string (Range.linear 1 10) Gen.alphaNum)
  otherHeaders <- fmap (DL.map (\(k, v) -> (CI.mk $ toS k, toS v))) $
                  Gen.list (Range.linear 0 10) $
                  genKvp 10 50
  secure_ <- Gen.bool
  pure Wai.defaultRequest { Wai.requestMethod = method_
                          , Wai.httpVersion = HT.http11
                          , Wai.rawPathInfo = toS path_
                          , Wai.rawQueryString = toS queryString_
                          , Wai.requestHeaders = ((maybeToList mCookieHeader) <> otherHeaders <> (maybeToList mHostHeader))
                          , Wai.isSecure = secure_
                          }

throwWithStack :: (MonadIO m, HasCallStack, Exception e) => e -> m a
throwWithStack e = UnliftIO.throwIO $ ExceptionWithCallStack e callStack

foo :: (HasCallStack) => Int -> IO ()
foo i =
  if i==5
  then throwWithStack DivideByZero -- error "foo error"
  else foo (i+1)

foo2 :: (HasCallStack) => IO ()
foo2 = UnliftIO.catch (foo 0) handler
  where
    -- handler :: (HasCallStack) => ExceptionWithCallStack -> IO ()
    -- handler x@(ExceptionWithCallStack e _) = do
    --   putStrLn $ show $ toSentry x
    handler :: (HasCallStack) => SomeException -> IO ()
    handler x = do
      putStrLn $ show $ toSentry x

withLocalTransport :: Sentry.SentryT IO a
                   -> IO (IORef [Event], a)
withLocalTransport action = do
  ref <- newIORef []
  svc <- Sentry.mkSentryService "http://edd7aa040752461e9f434724deb3dd03@168.119.172.134:9000/1" Prelude.id $
         (\_ evt -> void $ modifyIORef' ref (\x -> evt:x))
  (,) <$> (pure ref) <*> (runReaderT action svc)

assertFieldPresent :: (Show a, HasCallStack)
                   => String
                   -> (a -> Maybe b)
                   -> a
                   -> Assertion
assertFieldPresent msg accessor record =
  assertBool ("Unexpection Nothing - " <> msg <> ": " <> show record) $ isJust (accessor record)

assertSingle :: (HasCallStack, Show a) => String -> [a] -> IO a
assertSingle msg xs = case xs of
  [] -> assertFailure (msg <> ": unexpected empty list")
  [x] -> pure x
  _ -> assertFailure (msg <> ": not expecting more than one element: " <> show xs)

testWithExceptionStacktrace :: HasCallStack => TestEnv -> IO ()
testWithExceptionStacktrace TestEnv{..} = do
  evt <- Gen.sample $ genBasicEvent envTime
  (ref, _) <- withLocalTransport $ do
    UnliftIO.catch (liftIO $ foo 0) $ \(e :: ExceptionWithCallStack) -> do
      void $ Sentry.captureEvent evt{ evtException = [toSentry e] }
  readIORef ref >>= \case
    [] -> assertFailure "Event not captured"
    [Event{..}] -> do
      case evtException of
        [] -> assertFailure "Event should have exception"
        [ex] -> do
          assertFieldPresent "SentryException.exExceptionType" exExceptionType ex
          assertFieldPresent "SentryException.exModuleName" exModuleName ex
          case (exStacktrace ex) of
            Nothing -> assertFailure "SentryException should have exStacktrace"
            Just st -> case stFrames st of
              [] -> assertFailure "Stacktrace should have frames"
              _ -> forM_ (stFrames st) $ \fr-> do
                assertFieldPresent "Frame.frFilename" frFilename fr
                assertFieldPresent "Frame.frFunction" frFunction fr
                assertFieldPresent "Frame.frModule " frModule fr
                assertFieldPresent "Frame.frLineno" frLineno fr
                assertFieldPresent "Frame.frColno" frColno fr
                assertFieldPresent "Frame.frPackage" frPackage fr
        x -> assertFailure $ "How did more than one exception get capture? " <> show x
    x -> assertFailure $ "More than one event captured: " <> show x

testWithUser :: TestEnv -> IO ()
testWithUser TestEnv{..} = do
  evt <- Gen.sample $ genBasicEvent envTime
  user <- Gen.sample genUser
  (ref, _) <- withLocalTransport $ do
    Sentry.captureEvent evt{ evtUser = Just user }
  evt <- readIORef ref >>= assertSingle "Captured events"
  assertFieldPresent "Event.evtUser" evtUser evt

testWithWaiRequest :: TestEnv -> IO ()
testWithWaiRequest TestEnv{..} = do
  evt <- Gen.sample $ genBasicEvent envTime
  req <- Gen.sample genWaiRequest
  (ref, _) <- withLocalTransport $ do
    Sentry.captureEvent evt{ evtRequest = Just $ Sentry.fromWaiRequest req }
  readIORef ref >>= (assertSingle "Expecting a single event") >>= assertFieldPresent "Event.evtRequest" evtRequest

propConfigureScope :: (HasCallStack) => TestEnv -> Property
propConfigureScope TestEnv{..} = property $ do
  evt <- forAll $ genBasicEvent envTime
  scope <- forAll $ genScope False
  (ref, _) <- liftIO $ withLocalTransport $ do
    Sentry.configureScope (const scope)
    Sentry.captureEvent evt
  (readIORef ref) >>= tapAnnotateShow >>= \case
    [] -> annotate "No evets captured" >> failure
    [evt2] -> Sentry.applyToEvent scope evt === evt2
    _ -> annotate "Multiple events captured. Expecting only a single event" >> failure

-- propWithScope :: (HasCallStack) => TestEnv -> Property
-- propWithScope TestEnv{..} = property $ do
--   where
--     withScopeRecur n =
--       if n==0
--       then pure ()
--       else do scope <- forAll $ genScope False
--               Sentry.withScope (const scope) $ do
--                 Sentry.captureEvent 

--         withScopeRecur (n - 1)
--   let withScopeRecur n = 
--   scope <- forAll $ genScope False
--   (ref, _) <- liftIO $ withLocalTransport $ do
--     Sentry.configureScope (const scope)
--     Sentry.captureEvent evt
--   (readIORef ref) >>= tapAnnotateShow >>= \case
--     [] -> annotate "No evets captured" >> failure
--     [evt2] -> Sentry.applyToEvent scope evt === evt2
--     _ -> annotate "Multiple events captured. Expecting only a single event" >> failure
