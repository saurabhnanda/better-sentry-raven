{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

#if defined(DB_BEAM) || defined(DB_PSQL_SIMPLE)
#define HAS_DB 1
#endif

module Lib (
    main,
    -- * Exports to hide warnings
    withSMGen,
    ) where

import           Control.Concurrent         (myThreadId, threadCapability)
import           Control.Concurrent.MVar    (MVar, modifyMVarMasked, newMVar)
import           Data.Aeson                 (ToJSON (..), object, pairs, (.=))
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import qualified Data.Vector                as V
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Random.SplitMix     as SM

#ifdef HAS_DB
import           Data.Foldable              (for_)
import           Data.Int                   (Int32)
import           Data.List                  (sortOn)
import           Data.Pool                  (Pool, createPool, withResource)
import           Lucid                      hiding (for_)
import           Servant.HTML.Lucid
#endif

#ifdef DB_BEAM
import           Database.Beam
import           Database.Beam.Postgres
#endif

#ifdef DB_PSQL_SIMPLE
import           Control.Monad.IO.Class     (MonadIO (..))
import           Database.PostgreSQL.Simple
#endif

#ifdef SENTRY
import qualified Sentry.Types as Sentry
import qualified Sentry.HTTP as Sentry
import qualified Sentry.Blank as Sentry (blank)
import qualified Network.Wai as Wai
import qualified Data.Vault.Lazy as Vault
import qualified Data.UUID                        as UUID (toASCIIBytes, toString)
import qualified Data.UUID.V4                     as UUID (nextRandom)
-- import qualified System.UUID.V1 as UUID
-- import qualified Data.UUID as UUID
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Client (Manager)
import qualified Data.ByteString.Char8 as C8
import Control.Exception (SomeException)
import Data.IORef (newIORef)
#endif

-------------------------------------------------------------------------------
-- API Definition
-------------------------------------------------------------------------------

data Routes route = Routes
    { routePlaintext :: route :- "plaintext" :> Get '[PlainText] Text
    , routeJson      :: route :- "json"      :> Get '[JSON]      JsonData
#ifdef HAS_DB
    , routeDb        :: route :- "db"        :> Get '[JSON]      World
    , routeFortune   :: route :- "fortunes"   :> Get '[HTML]      FortunePage
#endif
    }
  deriving (Generic)

serverRoutes :: Ctx -> Routes AsServer
serverRoutes _ctx = Routes
    { routePlaintext = handlerPlaintext
    , routeJson      = handlerJson
#ifdef HAS_DB
    , routeDb        = handlerDb _ctx
    , routeFortune   = handlerFortune _ctx
#endif
    }

app :: Ctx -> Application
app = genericServe . serverRoutes


#ifdef SENTRY
mkSentryService :: Manager -> IO Sentry.SentryService
mkSentryService mgr =
  let evtDefaults evt = evt{ Sentry.evtLogger = Just "some-logger"
                           , Sentry.evtEnvironment = Just "test"
                           --, Sentry.evtTags = [("request_id", C8.unpack reqId)]
                           }
      fallbackTransport :: Sentry.Event -> SomeException -> IO ()
      fallbackTransport _ _ = pure ()
  in Sentry.mkSentryService "https://whatever@end4z5bumpvo7.x.pipedream.net/999999" evtDefaults $
     Sentry.mkHttpTransport True mgr fallbackTransport

mkSentryMiddleWare :: Vault.Key Sentry.SentryService
                   -> Sentry.SentryService
                   -> Wai.Middleware
mkSentryMiddleWare instrKey svc nextApp req respond = do
  reqId <- UUID.nextRandom
  scopeRef <- newIORef Sentry.blank -- { Sentry.scopeTags = Sentry.ScopeOpAdd [ ("request_id", UUID.toString reqId) ] }
  let newSvc = svc { Sentry.svcScopeRef = scopeRef }
      newReq = req { Wai.vault = Vault.insert instrKey newSvc (Wai.vault req) }
      addLogIdHeader = Wai.mapResponseHeaders (\respHeaders -> ("X-Request-Id", UUID.toASCIIBytes reqId):respHeaders)
  nextApp newReq (respond . addLogIdHeader)

main :: Int -> IO ()
main _cap = do
  putStrLn "Servant is ready to serve you"
  baseApp <- mkApp _cap
  instrKey <- Vault.newKey
  mgr <- getGlobalManager
  svc <- mkSentryService mgr
  let sentryMiddleware = mkSentryMiddleWare instrKey svc
  Warp.run 7041 $ sentryMiddleware baseApp

#else

main :: Int -> IO ()
main _cap = do
  putStrLn "Servant is ready to serve you"
  mkApp _cap >>= Warp.run 7041
#endif

-- | entry point
mkApp
    :: Int  -- ^ number of capabilities
    -> IO Application
mkApp _cap = do
    -- create 32 randon number generators
    rng <- V.replicateM 32 (SM.newSMGen >>= newMVar)

#ifdef HAS_DB
    db <- createPool
        (connect dbConnectInfo)
        close
        _cap
        0.5
        512
#endif

    let ctx = Ctx { ctxRng = rng
#ifdef HAS_DB
                  , ctxDb  = db
#endif
                  }
    pure $ app ctx
  where
#ifdef HAS_DB
    dbConnectInfo :: ConnectInfo
    dbConnectInfo = ConnectInfo
        { connectHost     = "tfb-database"
        , connectPort     = 5432
        , connectUser     = "benchmarkdbuser"
        , connectPassword = "benchmarkdbpass"
        , connectDatabase = "hello_world"
        }
#endif

-------------------------------------------------------------------------------
-- Execution context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxRng :: !(Vector (MVar SM.SMGen))
#ifdef HAS_DB
    , ctxDb  :: !(Pool Connection)
#endif
    }

#ifdef HAS_DB
withConnection :: Ctx -> (Connection -> IO r) -> IO r
withConnection ctx = withResource (ctxDb ctx)
#endif

withSMGen :: Ctx -> (SM.SMGen -> IO r) -> IO r
withSMGen ctx k = do
    tid <- myThreadId
    (cap, _) <- threadCapability tid
    gen <- modifyMVarMasked (ctxRng ctx V.! mod cap 32) $ return . SM.splitSMGen
    k gen

-------------------------------------------------------------------------------
-- Test 1: JSON serialization
-------------------------------------------------------------------------------

newtype JsonData = JsonData Text

instance ToJSON JsonData where
    toEncoding (JsonData t) = pairs ("message" .= t)
    toJSON     (JsonData t) = object [ "message" .= t ]


handlerJson :: Handler JsonData
handlerJson = return $ JsonData "Hello, World!"

-------------------------------------------------------------------------------
-- Test 2: Single database query
-------------------------------------------------------------------------------

#ifdef HAS_DB
handlerDb :: Ctx -> Handler World
#endif

#ifdef DB_BEAM
handlerDb ctx = liftIO $
    withConnection ctx $ \conn ->
    withSMGen ctx $ \gen -> do
        -- generate random id, first [0, 10000), then 'succ' to [1,10000]
        let (randomId', _) = SM.bitmaskWithRejection32 10000 gen
        let randomId :: Int32
            randomId = succ (fromIntegral randomId')

        ws <- runBeamPostgres conn $ runSelectReturningList $
            lookup_ (tfbWorld tfbDb) (WorldId randomId)

        case ws of
            (w:_) -> return w
            []    -> return $ World 0 0
#endif

#ifdef DB_PSQL_SIMPLE
handlerDb ctx = liftIO $
    withConnection ctx $ \conn ->
    withSMGen ctx $ \gen -> do
        -- generate random id, first [0, 10000), then 'succ' to [1,10000]
        let (randomId', _) = SM.bitmaskWithRejection32 10000 gen
        let randomId :: Int32
            randomId = succ (fromIntegral randomId')

        ws <- query conn "SELECT id, randomnumber FROM World where id = ?" (Only randomId)

        case ws of
            (w:_) -> return w
            []    -> return $ World 0 0
#endif

-------------------------------------------------------------------------------
-- Test 3: Multiple database query
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test 4: Fortunes
-------------------------------------------------------------------------------

#ifdef HAS_DB

newtype FortunePage = FortunePage (forall m. Monad m => HtmlT m ())

instance ToHtml FortunePage where
    toHtml = toHtmlRaw
    toHtmlRaw (FortunePage h) = h

renderFortunes :: [Fortune] -> FortunePage
renderFortunes fs = FortunePage $ doctypehtml_ $ do
    head_ $ title_ "Fortunes"
    body_ $ table_ $ do
        tr_ $ do
            th_ "id"
            th_ "message"
        for_ fs' $ \f -> tr_ $ do
            td_ $ toHtml $ show $ fortuneId f
            td_ $ toHtml $ fortuneMessage f
  where
    fs' = sortOn fortuneMessage $
        Fortune 0 "Additional fortune added at request time." : fs


handlerFortune :: Ctx -> Handler FortunePage
#endif

#ifdef DB_BEAM
handlerFortune ctx = liftIO $ withConnection ctx $ \conn -> do
    fs <- runBeamPostgres conn $ runSelectReturningList $ select $
        all_ (tfbFortune tfbDb)

    return $ renderFortunes fs
#endif

#ifdef DB_PSQL_SIMPLE
handlerFortune ctx = liftIO $ withConnection ctx $ \conn -> do
    fs <- query_ conn "SELECT id, message FROM Fortune;"
    return $ renderFortunes fs
#endif

-------------------------------------------------------------------------------
-- Test 5: Updates
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test 6: Plaintext endpoint
-------------------------------------------------------------------------------

handlerPlaintext :: Handler Text
handlerPlaintext = return "Hello, World!"

-------------------------------------------------------------------------------
-- beam
-------------------------------------------------------------------------------

#ifdef DB_BEAM
-- | World table.
data WorldT f = World
    { worldId           :: Columnar f Int32
    , worldRandomNumber :: Columnar f Int32
    }
  deriving (Generic, Beamable)

instance Table WorldT where
   data PrimaryKey WorldT f = WorldId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = WorldId . worldId

type World = WorldT Identity
-- type WorldId = PrimaryKey WorldT Identity

deriving instance Eq World
deriving instance Show World

-- | Fortune table.
data FortuneT f = Fortune
    { fortuneId      :: Columnar f Int32
    , fortuneMessage :: Columnar f Text
    }
  deriving (Generic, Beamable)

instance Table FortuneT where
   data PrimaryKey FortuneT f = FortuneId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = FortuneId . fortuneId

type Fortune = FortuneT Identity
-- type FortuneId = PrimaryKey FortuneT Identity

deriving instance Eq Fortune
deriving instance Show Fortune

-- Database definition.
data TfbDb f = TfbDb
    { tfbWorld   :: f (TableEntity WorldT)
    , tfbFortune :: f (TableEntity FortuneT)
    }
 deriving (Generic, Database Postgres)

tfbDb :: DatabaseSettings Postgres TfbDb
tfbDb = defaultDbSettings `withDbModification` modification where
    modification = (dbModification :: DatabaseModification (DatabaseEntity Postgres TfbDb) Postgres TfbDb)
        { tfbWorld = modifyEntityName (\_ -> "World") <> modifyTableFields tableModification
            { worldId           = "id"
            , worldRandomNumber = "randomnumber"
            }
        , tfbFortune = modifyEntityName (\_ -> "Fortune") <> modifyTableFields tableModification
            { fortuneId      = "id"
            , fortuneMessage = "message"
            }
        }
#endif

-------------------------------------------------------------------------------
-- postgresql-simple
-------------------------------------------------------------------------------

#ifdef DB_PSQL_SIMPLE
data World = World
    { worldId           :: Int32
    , worldRandomNumber :: Int32
    }
  deriving (Eq, Show, Generic, FromRow)

data Fortune = Fortune
    { fortuneId      :: Int32
    , fortuneMessage :: Text
    }
  deriving (Eq, Show, Generic, FromRow)
#endif

-------------------------------------------------------------------------------
-- DB common
-------------------------------------------------------------------------------

#ifdef HAS_DB
instance ToJSON World where
    toEncoding (World i rn) = pairs ("id" .= i <> "randomNumber" .= rn)
    toJSON     (World i rn) = object ["id" .= i, "randomNumber" .= rn]
#endif
