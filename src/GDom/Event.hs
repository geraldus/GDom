module GDom.Event
    (
      newEvent_
    , elementDispatchEvent
    ) where

import GHCJS.Foreign (ToJSString(..))
import GHCJS.Types (JSString)

import GDom.Types


newEvent_ :: ToJSString s => s -> IO DocumentEvent
newEvent_ = js_newEvent_ . toJSString

elementDispatchEvent :: DocumentElement -> DocumentEvent -> IO ()
elementDispatchEvent = js_elementDispatchEvent

foreign import javascript unsafe "$r = new Event($1);"
    js_newEvent_ :: JSString -> IO DocumentEvent

foreign import javascript unsafe "$1.dispatchEvent($2);"
    js_elementDispatchEvent :: DocumentElement -> DocumentEvent -> IO ()
