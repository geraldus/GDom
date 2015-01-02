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