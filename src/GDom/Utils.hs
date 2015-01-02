module GDom.Utils(
  getTimeZoneOffset
, tagNameFromStr
) where

import           Data.Char               (toUpper)
import           Data.Text               (Text)
import           GHCJS.Types             (JSRef(..))
import           GDom.Types              (HtmlTagName(..))

default (Text)

getTimeZoneOffset :: IO Int
getTimeZoneOffset = do
    d <- js_newDate
    js_getTimeZoneOffset d

-- FIXME Поддерживать все возможные ярлыки
tagNameFromStr :: String -> HtmlTagName
tagNameFromStr = fromUpperCase . map toUpper
    where fromUpperCase "A" = AnchorTagName
          fromUpperCase "DD" = DefinitionDescriptionTagName
          fromUpperCase "DL" = DefinitionListTagName
          fromUpperCase "DT" = DefinitionTermTagName
          fromUpperCase "DIV" = DivTagName
          fromUpperCase "FORM" = FormTagName
          fromUpperCase "INPUT" = InputTagName
          fromUpperCase "H1" = Header1TagName
          fromUpperCase "H2" = Header2TagName
          fromUpperCase "H3" = Header3TagName
          fromUpperCase "H4" = Header4TagName
          fromUpperCase "H5" = Header5TagName
          fromUpperCase "H6" = Header6TagName
          fromUpperCase "LABEL" = LabelTagName
          fromUpperCase "LI" = ListItemTagName
          fromUpperCase "OL" = OrderedListTagName
          fromUpperCase "P" = ParagraphTagName
          fromUpperCase "SPAN" = SpanTagName
          fromUpperCase "STRONG" = StrongTagName
          fromUpperCase "TABLE" = TableTagName
          fromUpperCase "THEAD" = TableHeadTagName
          fromUpperCase "TBODY" = TableBodyTagName
          fromUpperCase "TFOOTER" = TableFooterTagName
          fromUpperCase "TR" = TableRowTagName
          fromUpperCase "TH" = TableHeaderTagName
          fromUpperCase "TD" = TableDataTagName
          fromUpperCase "UL" = UnorderedListTagName
          fromUpperCase _ = CustomTagName


data TimeRef_ = TimeRef_
type TimeRef = JSRef TimeRef_

foreign import javascript safe "$r = new Date();"
    js_newDate :: IO (TimeRef)

foreign import javascript safe "$r = (-1) * $1.getTimezoneOffset();"
    js_getTimeZoneOffset :: TimeRef -> IO Int