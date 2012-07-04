module Main where

import Control.Monad.Trans (liftIO)
import Control.Concurrent (MVar, newMVar, readMVar, putMVar, takeMVar)

import qualified Network.WebSockets as WS
import qualified Data.ByteString as B
--import qualified Data.Text as T


data Client = Client { cSink :: WS.Sink WS.Hybi10 
                     }

data ServerState = ServerState { clients :: [Client]
                               }
newServerState :: ServerState
newServerState = ServerState { clients = []
                             }
wsReadBinary :: WS.WebSockets WS.Hybi10 B.ByteString
wsReadBinary = WS.receiveData

perClient :: MVar ServerState -> WS.WebSockets WS.Hybi10 ()
perClient mstate = do
  msg <- wsReadBinary
  liftIO $ print msg
  _ <- liftIO $ readMVar mstate
  WS.sendBinaryData msg
  perClient mstate
  

app :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
app mstate req = do
  WS.acceptRequest req
  WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
  sink <- WS.getSink
  let client = Client { cSink = sink }
  state <- liftIO $ takeMVar mstate
  liftIO $ putMVar mstate (state { clients = client : clients state })
  perClient mstate -- sink

main :: IO ()
main = do
  mstate <- newMVar newServerState
  WS.runServer "0.0.0.0" 8001 $ app mstate
  return ()


