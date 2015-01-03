module GDom.Utils(
  getTimeZoneOffset
, tagNameFromStr
, propCascadeMaybe, propCascadeMaybe'
) where

import           Control.Monad             (foldM)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Char                 (toUpper)
import           Data.Text                 (Text)
import           GHCJS.Foreign             (ToJSString(..), getPropMaybe)
import           GHCJS.Marshal             (FromJSRef(..))
import           GHCJS.Types               (JSRef(..))
import           GDom.Types                (HtmlTagName(..))
import           Unsafe.Coerce             (unsafeCoerce)

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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
propCascadeMaybe :: ToJSString prop
               => JSRef a -> [prop] -> IO (Maybe (JSRef b))
propCascadeMaybe el = runMaybeT . propCascadeMaybeTJSRef el

propCascadeMaybe' :: (FromJSRef res, ToJSString prop)
               => JSRef a -> [prop] -> IO (Maybe res)
propCascadeMaybe' el propCascade = runMaybeT $
        MaybeT . fromJSRef =<< propCascadeMaybeTJSRef el propCascade

-- propCascadeMaybeJSRef :: ToJSString prop =>
--                        JSRef a -> [prop] -> IO (Maybe (JSRef b))
-- propCascadeMaybeJSRef el = runMaybeT . propCascadeMaybeTJSRef el

propCascadeMaybeTJSRef :: ToJSString prop =>
                        JSRef a -> [prop] -> MaybeT IO (JSRef b)
propCascadeMaybeTJSRef el = unsafeCoerce . foldM foldChain el

foldChain :: ToJSString prop => JSRef a -> prop -> MaybeT IO (JSRef b)
foldChain obj = MaybeT . (`getPropMaybe` obj)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
foreign import javascript safe "$r = new Date();"
    js_newDate :: IO (TimeRef)

foreign import javascript safe "$r = (-1) * $1.getTimezoneOffset();"
    js_getTimeZoneOffset :: TimeRef -> IO Int