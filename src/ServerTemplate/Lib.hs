{-# LANGUAGE QuasiQuotes #-}

module ServerTemplate.Lib where

import Text.RawString.QQ
import Data.Text as T

libHs :: T.Text
libHs = T.pack $ [r|{-# LANGUAGE OverloadedStrings #-}
module Static.Lib
    ( mainServer
    ) where

import           Control.Concurrent             (forkIO,threadDelay)
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM         (TQueue, atomically, readTQueue,
                                                 writeTQueue, STM)
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Text              as T
import           Data.Text                      (Text)
import           Data.Text.IO                   as Tio
import qualified Data.ByteString.Lazy.Char8          as C
import           Network.HTTP.Types             (status200)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets (websocketsOr)

import           Text.Read                      (readMaybe)
import qualified Network.WebSockets             as WS
import System.Environment (getArgs)

import qualified Static.ServerLogic as ServerLogic
import Static.ServerTypes
import Types
import Static.Decode
import Utils.Utils


wsApp :: TQueue CentralMessage -> WS.ServerApp
wsApp centralMessageChan pendingConn = 
    let
        loop :: WS.Connection -> TQueue ClientThreadMessage -> IO ()
        loop conn clientMessageChan = do
                  -- wait for login message
                  Prelude.putStrLn "Waiting for version string from client..."
                  rawMsg <- WS.receiveData conn
                  Tio.putStrLn $ T.concat ["Got login message:", rawMsg]
                  
                  case rawMsg of 
                    "v0.1" -> do -- tell the central thread to log the user in
                        WS.sendTextData conn ("v" :: T.Text) --tell client that its version is right
                        atomically $ writeTQueue centralMessageChan (NewUser clientMessageChan conn)
                        forever $ threadDelay 10000000000 --sleep this thread forever (we need it to keep the connection alive)
                        return ()
                    _      -> do -- user's client version does not match 
                        WS.sendTextData conn ("v0.1" :: T.Text) --tell client that its version is wrong
                        WS.sendClose conn ("v0.1" :: T.Text) --close the Connection
                        return ()
    in
        do
        -- This function handles each new Connection.
        conn <- WS.acceptRequest pendingConn
        WS.forkPingThread conn 30

        -- Get a new message channel for this client.
        clientMessageChan <- atomically ServerLogic.newClientMessageChan

        loop conn clientMessageChan

fallbackApp :: TQueue CentralMessage -> Application
fallbackApp centralChan _ respond = do
    tempQ <- atomically $ giveReceivingQueue centralChan GetCurrentState
    state <- atomically $ readTQueue tempQ
    let userState = internalServerState state
    respond $ responseLBS status200 [] $ C.pack $ show userState


app :: TQueue CentralMessage -> Application
app centralChan = websocketsOr WS.defaultConnectionOptions (wsApp centralChan) (fallbackApp centralChan)


mainServer :: IO ()
mainServer = do
    args <- getArgs
    let port = if length args > 0 then (read $ (args !! 0) :: Int) else 8080
    centralMessageChan <- atomically ServerLogic.newCentralMessageChan
    forkIO $ ServerLogic.processCentralChan centralMessageChan
    Prelude.putStrLn $ "starting server on port " ++ show port
    run port (app centralMessageChan)|]