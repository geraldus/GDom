{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module GDom.WebSockets
( WebSocket
, WebSocketConnection
, WebSocketReadyState(..)
, MessageEvent(..)
, reactiveWebSocket
, wsClose
, wsSend
, wsOnMessage
, safeHttpToWsProtocol
) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Concurrent      (forkIO)
import           Data.Aeson
import           Data.List               (isPrefixOf)
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           FRP.Sodium
import           GHCJS.Foreign
import           GHCJS.Types             (JSRef(..), JSFun, JSString)
import           GHCJS.Marshal           (FromJSRef(..))
import           GDom.CommonDom          (reportError)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           GDom.CommonDom          (reportWarning)

default (Text)

type REvent a = (Event a, a -> Reactive ())
type RBehaviour a = (Behaviour a, a -> Reactive ())

data WebSocketConnection_ = WebSocketConnection_
type WebSocketConnection = JSRef WebSocketConnection_

newtype WebSocket = WebSocket { unWebSocket ::  WebSocketE }

data WebSocketE = WebSocketE
        { wsState      :: Behaviour WebSocketReadyState
        , wsMessages   :: Event MessageEvent
        , wsSendQ      :: BS.ByteString -> IO ()
        , wsConnection :: WebSocketConnection
        -- wsCleanUp      :: IO ()
        }

data WebSocketReadyState = WSConnecting
                         | WSOpen
                         | WSClosing
                         -- возможно, не имеет смысла? не отслеживается через
                         -- свойство WebSocket.readyState
                         | WSClosed
    deriving (Show, Eq)

data MessageEvent = MessageEvent
        { dataString :: BSL.ByteString
        , timeStamp  :: Double }
    deriving (Show)


{-
    Отзывчивое сетевое гнездо
    после создания такого гнезда есть следующие возможности
    * выполнять определённые действия при получении сообщения из гнезда
      wsOnMessage ws l :: WebSocket -> (MessageEvent -> IO ())
    * закрывать подключение
      wsColse w :: WebSocket -> IO ()
    * отправлять объекты, которые могут быть преобразованы в JSON вид
      wsSend w dat :: WebSocket -> obj -> IO (), где есть образец ToJSON obj
-}

reactiveWebSocket :: ToJSString a => a -> IO WebSocket
reactiveWebSocket url = do
    -- FIXME Чистка, освобождение при закрытии гнезда
    (msgEvt, readySRB, gateRE, qRB) <- sync $ do
        a <- newEvent                  -- Messages :: Event (JSRef m)
        b <- newBehaviour WSConnecting -- State :: Behaviour WebSocketReadyState
        c <- newEvent                  -- Send pull :: Event BS.ByteString
        d <- newBehaviour []           -- Send Q    :: Behaviour BS.ByteString
        return (a,b,c,d)
    let (msgE, msgF)       = msgEvt
        (readySB, readySF) = readySRB
        (gateE, gateF)     = gateRE
        (qB, qF)           = qRB
        datE = snapshot (flip (,)) gateE readySB

        dat2Q (state,dat)
            | state == WSConnecting || state == WSOpen = do
                 forkIO . sync $ do
                     q' <- sample qB
                     qF (dat:q')
                 return ()
            | otherwise = reportWarning "Websocket is closing or already closed"

    let sendQ bs = do
            forkIO . sync . gateF $ bs
            return ()

    onMessage <- syncCallback1 NeverRetain False (readMsg (sync . msgF))
    onOpen    <- syncCallback  NeverRetain False (sync $ readySF WSOpen)
    onClose   <- syncCallback  NeverRetain False (sync $ readySF WSClosed)

    conn <- js_newWebSocket (toJSString url) onMessage onOpen onClose

    let onStateChange WSOpen = do
            q <- sync $ do
                 x <- sample qB
                 qF []
                 return x
            mapM_ (\x -> do
                      forkIO . wsSendData conn $ x
                      return ()) q
        onStateChange WSConnecting = return ()
        onStateChange _ = do
            reportWarning
                "Websocket is closing or already closed. Purging queue"
            sync . qF $ []

    sync $ do
        listen datE dat2Q
        listen (value readySB) onStateChange

    return . WebSocket
           $ WebSocketE readySB msgE sendQ conn

  where readMsg :: (MessageEvent -> IO()) -> JSRef MessageEvent -> IO ()
        readMsg f ref = do
            m <- fromJSRef ref
            case m of
                Just evt -> f evt
                _  -> reportError "Unexpected message from web socket recieved."

        wsSendData ::  WebSocketConnection -> BS.ByteString -> IO ()
        wsSendData c = js_webSocketSend c . toJSString . decodeUtf8

wsSend :: ToJSON dat => WebSocket -> dat -> IO ()
wsSend ws = (wsSendQ . unWebSocket $ ws) . BSL.toStrict . encode

wsOnMessage :: WebSocket -> (MessageEvent -> IO ()) -> IO ()
wsOnMessage w f = do
    sync . listen (wsMessages . unWebSocket $ w) $  f'
    return ()
  where f' x = forkIO (f x) >> return ()

wsClose :: WebSocket -> IO ()
wsClose = js_webSocketClose . wsConnection . unWebSocket
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
foreign import javascript unsafe "$r = (function () {\
        \var conn = new WebSocket($1);\
        \conn.onmessage = $2;\
        \conn.onopen = $3;\
        \conn.onclose = $4;\
        \return conn;})()"
    js_newWebSocket :: JSString
                    -> JSFun (JSRef MessageEvent -> IO ())
                    -> JSFun (IO ())
                    -> JSFun (IO ())
                    -> IO WebSocketConnection

foreign import javascript unsafe "$1.send($2);"
    js_webSocketSend :: WebSocketConnection -> JSString -> IO ()

foreign import javascript unsafe "$1.close();"
    js_webSocketClose :: WebSocketConnection -> IO ()
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
safeHttpToWsProtocol :: String -> Maybe String
safeHttpToWsProtocol url = stripHttp url >>=
        stripRest >>=
        \(s, u) -> Just $ if s then "wss" ++ u else "ws" ++ u
    where
        stripHttp :: String -> Maybe String
        stripHttp s = if "http" `isPrefixOf` s
            then Just (drop 4 s)
            else Nothing

        stripRest :: String -> Maybe (Bool, String)
        stripRest "" = Nothing
        stripRest s =
            let (isSafe, s') = case head s of
                    's' -> (True, drop 1 s)
                    _   -> (False, s)
            in if "://" `isPrefixOf` s'
                then Just (isSafe, s')
                else Nothing
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
instance FromJSRef BSL.ByteString where
    fromJSRef x = do
        typ <- typeOf x
        case typ of
            4 -> fromJSRef x
            _ -> return Nothing

instance FromJSRef MessageEvent where
    fromJSRef m = do
        typ <- typeOf m
        case typ of
            6 -> do -- 6 обозначает Объект Ява—письменности
                md <- getPropMaybe ("data" :: String) m
                mt <- getPropMaybe ("timeStamp" :: String) m
                d <- case md of
                    Just dataJS -> fromJSRef dataJS
                    Nothing -> return Nothing
                t <- case mt of
                    Just timeStampJS -> fromJSRef timeStampJS
                    Nothing -> return Nothing
                return $ MessageEvent <$> d <*> t
            _ -> return Nothing
--------------------------------------------------------------------------------
