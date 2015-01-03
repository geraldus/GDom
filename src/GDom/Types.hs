{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module GDom.Types where

import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Text                 (Text)
import           GHCJS.Foreign             (getPropMaybe,typeOf)
import           GHCJS.Marshal             (FromJSRef(..))
import           GHCJS.Types               (JSRef(..), JSArray)

default (Text)


data DocumentElement_ = DocumentElement_
type DocumentElement = JSRef DocumentElement_

type DocumentElementCollection = JSArray DocumentElement_

type WithElement = DocumentElement -> IO DocumentElement
type OnElement = DocumentElement -> IO ()

-- Имя ярлыка и сущность ярлыка будут описаны раздельно, так как ярлык в широком
-- понимании, это не просто имя (к примеру, DivTag), а некая сущность, которая
-- обладает свойствами и имеет определённое состояние, которое может меняться
-- с течением времени
-- FIXME Поддерживать все возможные ярлыки
data HtmlTagName = AnchorTagName
                 | DefinitionDescriptionTagName
                 | DefinitionListTagName
                 | DefinitionTermTagName
                 | DivTagName
                 | FormTagName
                 | InputTagName
                 | Header1TagName
                 | Header2TagName
                 | Header3TagName
                 | Header4TagName
                 | Header5TagName
                 | Header6TagName
                 | LabelTagName
                 | ListItemTagName
                 | OrderedListTagName
                 | ParagraphTagName
                 | SpanTagName
                 | StrongTagName
                 | TableTagName
                 | TableHeadTagName
                 | TableBodyTagName
                 | TableFooterTagName
                 | TableRowTagName
                 | TableHeaderTagName
                 | TableDataTagName
                 | UnorderedListTagName
                 | CustomTagName
                 -- ^ уточнить какие имена получают собственные ярлыки,
                 --   присутствующие в документе
    deriving (Show, Read, Eq, Bounded)


data DocumentEvent_ = AnyEvent
type DocumentEvent = JSRef DocumentEvent_


type WindowLocationRef = JSRef WindowLocationState

data WindowLocationState = WindowLocationState
        { wlsHref     :: UrlUtilsHref
        , wlsProtocol :: HttpProtocol
        , wlsHost     :: UrlUtilsHref
        , wlsHostName :: UrlUtilsHostName
        , wlsPort     :: UrlUtilsPort
        , wlsPathName :: UrlUtilsPathName
        , wlsSearch   :: UrlUtilsSearch
        , wlsHash     :: UrlUtilsHash
        , wlsUserName :: UrlUtilsUserName
        , wlsPassword :: UrlUtilsPassword
        , wlsOrigin   :: UrlUtilsOrigin
        }
        deriving (Show, Eq)

data HttpProtocol = HttpProtocol
                  | WsProtocol
                  | SecuredHttpProtocol
                  | SecuredWsProtocol
                  deriving (Show, Eq)

type UrlUtilsHref     = Text
type UrlUtilsProtocol = Text
type UrlUtilsHost     = Text
type UrlUtilsHostName = Text
type UrlUtilsPort     = Text
type UrlUtilsPathName = Text
type UrlUtilsSearch   = Text
type UrlUtilsHash     = Text
type UrlUtilsUserName = Text
type UrlUtilsPassword = Text
type UrlUtilsOrigin   = Text

instance FromJSRef WindowLocationState where
    fromJSRef x = do
        ty <- typeOf x
        case ty of
            6 -> runMaybeT $ do
                href <- MaybeT . getPropFromJSRef "href" $ x
                protocolTxt <- MaybeT . getPropFromJSRef "protocol" $ x
                protocol <- maybeT . safeProtocolFromText $ protocolTxt
                host <- MaybeT . getPropFromJSRef "host" $ x
                hostName <- MaybeT . getPropFromJSRef "hostname" $ x
                port <- MaybeT . getPropFromJSRef "port" $ x
                pathName <- MaybeT . getPropFromJSRef "pathname" $ x
                search <- MaybeT . getPropFromJSRef "search" $ x
                hash <- MaybeT . getPropFromJSRef "hash" $ x
                userName <- MaybeT . getPropFromJSRef "username" $ x
                password <- MaybeT . getPropFromJSRef "password" $ x
                origin <- MaybeT . getPropFromJSRef "origin" $ x
                return WindowLocationState
                        { wlsHref     = href
                        , wlsProtocol = protocol
                        , wlsHost     = host
                        , wlsHostName = hostName
                        , wlsPort     = port
                        , wlsPathName = pathName
                        , wlsSearch   = search
                        , wlsHash     = hash
                        , wlsUserName = userName
                        , wlsPassword = password
                        , wlsOrigin   = origin
                        }
            _ -> return Nothing
      where getPropFromJSRef prop el = runMaybeT $ do
                propRef <- MaybeT . getPropMaybe prop $ el
                propVal <- MaybeT . fromJSRef $ propRef
                return propVal
            maybeT = MaybeT . return

protocolToText :: HttpProtocol -> Text
protocolToText HttpProtocol        = "http:"
protocolToText SecuredHttpProtocol = "https:"
protocolToText WsProtocol          = "ws:"
protocolToText SecuredWsProtocol   = "wss:"

safeProtocolFromText :: Text -> Maybe HttpProtocol
safeProtocolFromText "http:"  = Just HttpProtocol
safeProtocolFromText "https:" = Just SecuredHttpProtocol
safeProtocolFromText "ws:"    = Just WsProtocol
safeProtocolFromText "wss:"   = Just SecuredWsProtocol
safeProtocolFromText _        = Nothing
