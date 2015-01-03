module GDom.Types where

import           GHCJS.Types             (JSRef(..), JSArray)


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
