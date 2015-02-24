{-# LANGUAGE OverloadedStrings #-}
module GDom.WebSockets
( WebSocket(..)
, WebSocketConnection
, WebSocketReadyState(..)
, MessageEvent(..)
, reactiveWebSocket
, safeHttpToWsProtocol
) where

import           Control.Applicative     ((<$>), (<*>))
import           Data.List               (isPrefixOf)
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           FRP.Sodium
import           GHCJS.Foreign
import           GHCJS.Types             (JSRef(..), JSFun, JSString)
import           GHCJS.Marshal           (FromJSRef(..), fromJSRef_fromJSString)
import           GDom.CommonDom          (reportError)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

default (Text)

data WebSocketConnection_ = WebSocketConnection_
type WebSocketConnection = JSRef WebSocketConnection_

data WebSocket = WebSocket
        { wsMessages   :: Event MessageEvent
        , wsState      :: Behaviour WebSocketReadyState
        , wsConnection :: WebSocketConnection
        , wsSend       :: BSL.ByteString -> IO ()
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


reactiveWebSocket :: ToJSString a => a -> IO WebSocket
reactiveWebSocket url = do
    (msgEvt, rstBhv) <- sync $ do
        a <- newEvent
        b <- newBehaviour WSConnecting
        return (a,b)
    let (msgE, msgF) = msgEvt
        (rstB, rstF) = rstBhv

    onMessage <- syncCallback1 NeverRetain False (readMsg (sync . msgF))
    onOpen    <- syncCallback  NeverRetain False (sync $ rstF WSOpen)
    onClose   <- syncCallback  NeverRetain False (sync $ rstF WSClosed)

    conn <- js_newWebSocket (toJSString url) onMessage onOpen onClose
    return $ WebSocket msgE rstB conn (sendData conn)
    where
        readMsg :: (MessageEvent -> IO()) -> JSRef MessageEvent -> IO ()
        readMsg f ref = do
            m <- fromJSRef ref
            case m of
                Just evt -> f evt
                _  -> reportError "Unexpected message from web socket recieved."

sendData ::  WebSocketConnection -> BSL.ByteString -> IO ()
sendData c d = js_webSocketSend c (toJSString d)


--------------------------------------------------------------------------------
foreign import javascript safe "$r = (function () {\
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

foreign import javascript safe "$1.send($2);"
    js_webSocketSend :: WebSocketConnection -> JSString -> IO ()
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
instance FromJSString BS.ByteString where
    fromJSString = encodeUtf8 . fromJSString

instance FromJSString BSL.ByteString where
    fromJSString = BSL.fromStrict . fromJSString
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
instance ToJSString BS.ByteString where
    toJSString = toJSString . decodeUtf8

instance ToJSString BSL.ByteString where
    toJSString = toJSString . BSL.toStrict
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
instance FromJSRef BSL.ByteString where
    fromJSRef x = do
        typ <- typeOf x
        case typ of
            4 -> fromJSRef_fromJSString x
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
