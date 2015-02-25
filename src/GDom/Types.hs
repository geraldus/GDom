{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module GDom.Types where

import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Text                 (Text)
import           GHCJS.Foreign             (getPropMaybe,typeOf)
import           GHCJS.Marshal             (FromJSRef(..))
import           GHCJS.Types               (JSRef(..), JSArray, castRef)
import           Text.Shakespeare.I18N     (ToMessage(..))

default (Text)


--------------------------------------------------------------------------------
data DocumentElement_ = DocumentElement_
type DocumentElement = JSRef DocumentElement_

type DocumentElementCollection = JSArray DocumentElement

type WithElement = DocumentElement -> IO DocumentElement
type OnElement = DocumentElement -> IO ()
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Имя ярлыка и сущность ярлыка будут описаны раздельно, так как ярлык в широком
-- понимании, это не просто имя (к примеру, DivTag), а некая сущность, которая
-- обладает свойствами и имеет определённое состояние, которое может меняться
-- с течением времени
-- FIXME Поддерживать все возможные ярлыки
data HtmlTagName = AnchorTagName
                 | BodyTagName
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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data InputType = ButtonIt
               | CheckBoxIt
               | ColorIt
               | DateIt
               | DateTimeIt
               | DateTimeLocalIt
               | EmailIt
               | FileIt
               | HiddenIt
               | ImageIt
               | MonthIt
               | NumberIt
               | PasswordIt
               | RadioIt
               | RangeIt
               | ResetIt
               | SearchIt
               | SubmitIt
               | TelIt
               | TextIt
               | TimeIt
               | UrlIt
               | WeekIt
               deriving (Show, Eq)

instance FromJSRef InputType where
    fromJSRef i = do
        mval <- fromJSRef . castRef $ i
        return $ case mval of
            Just tval -> safeInputTypeFromText tval
            Nothing -> Nothing
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data DocumentEvent_ = AnyEvent
type DocumentEvent = JSRef DocumentEvent_
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
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
        , wlsUserName :: Maybe UrlUtilsUserName
        , wlsPassword :: Maybe UrlUtilsPassword
        , wlsOrigin   :: UrlUtilsOrigin
        }
        deriving (Show, Eq)

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
                userName <- lift . getPropFromJSRef "username" $ x
                password <- lift . getPropFromJSRef "password" $ x
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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
instance ToMessage InputType where
    toMessage ButtonIt = "button"
    toMessage CheckBoxIt = "checkbox"
    toMessage ColorIt = "color"
    toMessage DateIt = "date"
    toMessage DateTimeIt = "datetime"
    toMessage DateTimeLocalIt = "datetime-local"
    toMessage EmailIt = "email"
    toMessage FileIt = "file"
    toMessage HiddenIt = "hidden"
    toMessage ImageIt = "image"
    toMessage MonthIt = "month"
    toMessage NumberIt = "number"
    toMessage PasswordIt = "password"
    toMessage RadioIt = "radio"
    toMessage RangeIt = "range"
    toMessage ResetIt = "reset"
    toMessage SearchIt = "search"
    toMessage SubmitIt = "submit"
    toMessage TelIt = "tel"
    toMessage TextIt = "text"
    toMessage TimeIt = "time"
    toMessage UrlIt = "url"
    toMessage WeekIt = "week"

safeInputTypeFromText :: Text -> Maybe InputType
safeInputTypeFromText "button" = Just ButtonIt
safeInputTypeFromText "checkbox" = Just CheckBoxIt
safeInputTypeFromText "color" = Just ColorIt
safeInputTypeFromText "date" = Just DateIt
safeInputTypeFromText "datetime" = Just DateTimeIt
safeInputTypeFromText "datetime-local" = Just DateTimeLocalIt
safeInputTypeFromText "email" = Just EmailIt
safeInputTypeFromText "file" = Just FileIt
safeInputTypeFromText "hidden" = Just HiddenIt
safeInputTypeFromText "image" = Just ImageIt
safeInputTypeFromText "month" = Just MonthIt
safeInputTypeFromText "number" = Just NumberIt
safeInputTypeFromText "password" = Just PasswordIt
safeInputTypeFromText "radio" = Just RadioIt
safeInputTypeFromText "range" = Just RangeIt
safeInputTypeFromText "reset" = Just ResetIt
safeInputTypeFromText "search" = Just SearchIt
safeInputTypeFromText "submit" = Just SubmitIt
safeInputTypeFromText "tel" = Just TelIt
safeInputTypeFromText "text" = Just TextIt
safeInputTypeFromText "time" = Just TimeIt
safeInputTypeFromText "url" = Just UrlIt
safeInputTypeFromText "week" = Just WeekIt
safeInputTypeFromText _ = Nothing
--------------------------------------------------------------------------------
