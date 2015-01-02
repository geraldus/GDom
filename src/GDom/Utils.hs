module GDom.Utils(
  getTimeZoneOffset
) where

import           Data.Text               (Text)
import           GHCJS.Types             (JSRef(..))

default (Text)

getTimeZoneOffset :: IO Int
getTimeZoneOffset = do
    d <- js_newDate
    js_getTimeZoneOffset d


data TimeRef_ = TimeRef_
type TimeRef = JSRef TimeRef_

foreign import javascript safe "$r = new Date();"
    js_newDate :: IO (TimeRef)

foreign import javascript safe "$r = (-1) * $1.getTimezoneOffset();"
    js_getTimeZoneOffset :: TimeRef -> IO Int