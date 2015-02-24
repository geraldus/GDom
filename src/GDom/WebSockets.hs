{-# LANGUAGE OverloadedStrings #-}
module GDom.WebSockets
( WebSocket(..)
, WebSocketConnection
, WebSocketReadyState(..)
, MessageEvent(..)
, reactiveWebSocket
, wsSendJSON
, safeHttpToWsProtocol
) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (liftM)
import           Data.Aeson              (ToJSON(..),encode)
import           Data.List               (isPrefixOf)
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           FRP.Sodium
import           GHCJS.Foreign
import           GHCJS.Types             (JSRef(..), JSFun, JSString)
import           GHCJS.Marshal           (FromJSRef(..))
import           GDom.CommonDom          (reportError)
import qualified Data.ByteString.Lazy as BSL

default (Text)

data WebSocketConnection_ = WebSocketConnection_
type WebSocketConnection = JSRef WebSocketConnection_

data WebSocket = WebSocket
        { wsOnMessage  :: (MessageEvent -> IO ()) -> IO ()
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
    (msgRE, rstBhv) <- sync $ do
        a <- newEvent
        b <- newBehaviour WSConnecting
        return (a,b)
    let (msgE, msgF) = msgRE
        (rstB, rstF) = rstBhv

    cbMessage <- syncCallback1 NeverRetain False (readMsg (sync . msgF))
    cbOpen    <- syncCallback  NeverRetain False (sync $ rstF WSOpen)
    cbClose   <- syncCallback  NeverRetain False (sync $ rstF WSClosed)

    conn <- js_newWebSocket (toJSString url) cbMessage cbOpen cbClose
    return $ WebSocket (onMessage msgE) rstB conn (sendData conn)
  where readMsg :: (MessageEvent -> IO()) -> JSRef MessageEvent -> IO ()
        readMsg f ref = do
            m <- fromJSRef ref
            case m of
                Just evt -> f evt
                _  -> reportError "Unexpected message from web socket recieved."

        sendData ::  WebSocketConnection -> BSL.ByteString -> IO ()
        sendData c d =
            js_webSocketSend c (toJSString . decodeUtf8 . BSL.toStrict $ d)
        onMessage :: Event MessageEvent -> (MessageEvent -> IO ()) -> IO ()
        onMessage e h = do
           sync . listen e $ h
           return ()

wsSendJSON :: ToJSON dat => WebSocket -> dat -> IO ()
wsSendJSON ws = wsSend ws . encode
--------------------------------------------------------------------------------


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
instance FromJSRef MessageEvent where
    fromJSRef m = do
        typ <- typeOf m
        case typ of
            6 -> do -- 6 обозначает Объект Ява—письменности
                md <- getPropMaybe ("data" :: String) m
                mt <- getPropMaybe ("timeStamp" :: String) m
                d <- case md of
                    Just dataJS -> do
                        x <- fromJSRef dataJS
                        return $ liftM (BSL.fromStrict . encodeUtf8) x
                    Nothing -> return Nothing
                t <- case mt of
                    Just timeStampJS -> fromJSRef timeStampJS
                    Nothing -> return Nothing
                return $ MessageEvent <$> d <*> t
            _ -> return Nothing
--------------------------------------------------------------------------------
