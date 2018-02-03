{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Reader
import           Data.Semigroup
import           Language.Haskell.TH
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import qualified ProjectM36.Client                    as C
import           ProjectM36.Client.Simple
import           Projectr.API
import           Servant
import           System.Environment

main :: IO ()
main = run 8080 . logStdoutDev . app $ AppContext "../test"

app :: AppContext -> Application
app = serve (Proxy @API) . server

server :: AppContext -> Server API
server context = enter nt $ questionImpl :<|> frontendApp
 where
   nt = runReaderTNat context :: App :~> Handler

questionImpl :: ServerT QuestionAPI App
questionImpl title = pure $ Question title ""

-- createSchema :: Db ()
-- createSchema = _

withMasterSession :: FilePath -> Db a -> IO (Either DbError a)
withMasterSession file f = do
  let connInfo =
        InProcessConnectionInfo
          (CrashSafePersistence file)
          emptyNotificationCallback
          []
  (sessionId, conn) <-
    either (error . show) id <$> simpleConnectProjectM36 connInfo
  either (Left . RelError) Right <$>
    C.withTransaction
      sessionId
      conn
      (runReaderT (runDb f) (sessionId, conn) >>= pure . Right)
      (C.commit sessionId conn)

frontendApp :: ServerT Raw App
frontendApp =
  ghcjsApp $
  $(litE . StringL =<< runIO (getEnv "PROJECTR_FRONTEND")) <>
  "/bin/projectr-frontend.jsexe/"

ghcjsApp :: FilePath -> ServerT Raw App
ghcjsApp dir =
  enter liftHandlerNat $
  serveDirectoryWith $
  (defaultWebAppSettings dir)
  {ss404Handler = Just (indexApp dir)}

indexApp :: FilePath -> Application
indexApp appDir _ respond =
  respond $
  responseFile
    status200
    [ (hCacheControl, "no-cache, no-store, must-revalidate, max-age=0")
    , (hPragma, "no-cache")
    , (hExpires, "Tue, 1 May 1985 01:10:00 GMT")
    ]
    (appDir <> "index.html")
    Nothing

liftHandlerNat :: Handler :~> App
liftHandlerNat = liftNat

type App = ReaderT AppContext Handler

data AppContext = AppContext
  { acDatabaseDir :: FilePath
  }
