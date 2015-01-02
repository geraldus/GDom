module GDom.CommonDom
( DocumentElement
, DocumentElementCollection
, DocumentEvent
, WithElement
, OnElement
, report
, reportWarning
, reportError
, reportInstance
, documentRef
, documentBody
, safeGetDocumentElement
, safeQuerySelector
, createElement
, innerTextOf
, setInnerText
, setInnerHtml
, tagName
, appendTo, prependTo, replaceChild
, querySelectorAll
, classListContains
, addClassName, addClassName_
, removeClassName, removeClassName_
, setAttribute, setAttribute_
, removeAttribute, removeAttribute_
, safeGetAttribute
, setDataAttribute, setDataAttribute_
, removeDataAttribute, removeDataAttribute_
, safeGetDataAttribute
, attachHandler, attachCapturingHandler, dispatchEvent
, newEvent, newCustomEvent, stopPropagation, stopImmediatePropagation
, setOnInput
, preventDefaultOf
, removeNode
, cloneNode, deepClone
, js_window, js_windowSafe
)
where

import           Data.Text               (Text)
import           GHCJS.Foreign
import           GHCJS.Types
import           GDom.Types
import           GDom.Utils              (tagNameFromStr)


--------------------------------------------------------------------------------
report :: ToJSString a => a -> IO ()
report = js_consoleLog . toJSString

reportError :: ToJSString a => a -> IO ()
reportError = js_consoleError . toJSString

reportWarning :: ToJSString a => a -> IO ()
reportWarning = js_consoleWarn . toJSString

reportInstance :: JSRef a -> IO ()
reportInstance = js_consoleLogObj
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
documentRef :: DocumentElement
documentRef = js_document

documentBody :: DocumentElement
documentBody = js_documentBody
{-# WARNING documentBody, documentRef
    "These coulud become IO operations because of mutable nature."
    #-}

safeGetDocumentElement :: ToJSString a => a -> IO (Maybe DocumentElement)
safeGetDocumentElement eid = do
    r <- js_getDocumentElement (toJSString eid)
    return $ if (isNull r)
        then Nothing
        else Just r

safeQuerySelector :: ToJSString a
                  => DocumentElement
                  -> a
                  -> IO (Maybe DocumentElement)
safeQuerySelector h q = do
    r <- js_querySelector h (toJSString q)
    return $ if isNull r
        then Nothing
        else Just r

querySelectorAll :: ToJSString a => DocumentElement -> a -> IO [DocumentElement]
querySelectorAll h xp = js_querySelectorAll h (toJSString xp) >>= fromArray
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
createElement :: ToJSString a => a -> IO DocumentElement
createElement = js_documentCreateElement . toJSString

removeNode :: OnElement
removeNode = js_remove

cloneNode :: DocumentElement -> Bool -> IO DocumentElement
cloneNode n = js_cloneNode n . toJSBool

deepClone :: WithElement
deepClone = flip cloneNode True

appendTo :: DocumentElement -> DocumentElement -> IO ()
appendTo = js_appendChild

prependTo :: DocumentElement -> DocumentElement -> IO ()
prependTo = js_prependTo

replaceChild :: DocumentElement -> DocumentElement -> DocumentElement -> IO ()
replaceChild = js_replaceChild
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
setInnerHtml :: ToJSString a => DocumentElement -> a -> IO ()
setInnerHtml e t = js_setInnerHtml e (toJSString t)

innerTextOf :: DocumentElement -> IO Text
innerTextOf e = js_getInnerText e >>= return . fromJSString

setInnerText :: ToJSString a => DocumentElement -> a -> IO ()
setInnerText e t = js_setInnerText e (toJSString t)
{-# WARNING innerTextOf, setInnerText
    "innerText is not cross-platform property" #-}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
addClassName :: ToJSString a => DocumentElement -> a -> IO DocumentElement
addClassName e cs = do
    addClassName_ e cs
    return e
addClassName_ :: ToJSString a => DocumentElement -> a -> IO ()
addClassName_ e = js_classListAdd e . toJSString

removeClassName :: ToJSString a => DocumentElement -> a -> IO DocumentElement
removeClassName e cs = do
    removeClassName_ e cs
    return e
removeClassName_ :: ToJSString a => DocumentElement -> a -> IO ()
removeClassName_ e = js_classListRemove e . toJSString

classListContains :: ToJSString a => DocumentElement -> a -> IO Bool
classListContains el c = (return . fromJSBool) =<<
        js_classListContains el (toJSString c)

safeGetAttribute :: (ToJSString a, FromJSString b)
                 => DocumentElement
                 -> a
                 -> IO (Maybe b)
safeGetAttribute e a = do
    x <- js_getAttribute e (toJSString a)
    return $ if (isNull x)
        then Nothing
        else Just (fromJSString x)

setAttribute :: ToJSString a => DocumentElement -> a -> a -> IO DocumentElement
setAttribute e a v = do
    js_setAttribute e (toJSString a) (toJSString v)
    return e
setAttribute_ :: ToJSString a => DocumentElement -> a -> a -> IO ()
setAttribute_ e a v = setAttribute e a v >> return ()

removeAttribute :: ToJSString a => DocumentElement -> a -> IO DocumentElement
removeAttribute e a = do
    e `js_removeAttribute` (toJSString a)
    return e
removeAttribute_ :: ToJSString a => DocumentElement -> a -> IO ()
removeAttribute_ e a = removeAttribute e a >> return ()

safeGetDataAttribute :: ToJSString a =>
                        DocumentElement -> a -> IO (Maybe Text)
safeGetDataAttribute e k = do
    nattr <- js_getDataAttribute e (toJSString k)
    return $ if isNull nattr
        then Nothing
        else Just (fromJSString nattr)

setDataAttribute_ :: (ToJSString a, ToJSString b) =>
                     DocumentElement -> a -> b -> IO ()
setDataAttribute_ e k v = js_setDataAttribute e (toJSString k) (toJSString v)
setDataAttribute :: (ToJSString a, ToJSString b) =>
                    DocumentElement -> a -> b -> IO DocumentElement
setDataAttribute e k v = setDataAttribute_ e k v >> return e

removeDataAttribute_ :: ToJSString a => DocumentElement -> a -> IO ()
removeDataAttribute_ e k = js_removeDataAttribute e (toJSString k)
removeDataAttribute :: ToJSString a =>
                       DocumentElement -> a -> IO DocumentElement
removeDataAttribute e k = removeDataAttribute_ e k >> return e

-- Текущий вариант описания чистый, так как имя ярлыка не может измениться с
-- течением времени. Каждый элемент документа обладает свойством ИмяЯрлыка.
-- Однако, следует помнить, что в сегодняшнем виде есть возможность вместо
-- реального объекта ЭлементДокумента всем описанным функциям передать
-- нереальный объект, к примеру, null или любой другой указатель легко можно
-- замаскировать под DocumentElement
tagName :: DocumentElement -> HtmlTagName
tagName =  tagNameFromStr . fromJSString . js_tagName
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
newEvent :: ToJSString a => a -> IO DocumentEvent
newEvent = js_newEvent . toJSString

newCustomEvent :: ToJSString a => a -> JSRef b -> IO DocumentEvent
newCustomEvent typ details = js_newCustomEvent (toJSString typ) details

dispatchEvent :: DocumentElement -> JSRef a -> IO ()
dispatchEvent = js_dispatchEvent

preventDefaultOf :: DocumentEvent -> IO ()
preventDefaultOf = js_eventPreventDefault

stopPropagation :: DocumentEvent -> IO ()
stopPropagation = js_eventStopPropagation

stopImmediatePropagation :: DocumentEvent -> IO ()
stopImmediatePropagation = js_eventStopImmediatePropagation

attachHandler :: ToJSString a =>
                 DocumentElement -> a -> (DocumentEvent -> IO ()) -> IO ()
attachHandler el evtp hnd = do
    callback <- syncCallback1 AlwaysRetain False hnd
    js_addEventListener el (toJSString evtp) callback (toJSBool False)

attachCapturingHandler :: ToJSString a
                       => DocumentElement
                       -> a
                       -> (DocumentEvent -> IO ())
                       -> IO ()
attachCapturingHandler el evtp hnd = do
    callback <- syncCallback1 AlwaysRetain False hnd
    js_addEventListener el (toJSString evtp) callback (toJSBool True)

setOnInput :: DocumentElement
        -> (DocumentElement -> IO ())
        -> IO ()
setOnInput e h = do
    f <- syncCallback AlwaysRetain False (h e)
    js_setOnInput e f
{-# DEPRECATED setOnInput
    "Use `addEventListener <element> \"input\"` instead." #-}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
foreign import javascript safe "console.error($1)"
    js_consoleError :: JSString -> IO ()

foreign import javascript safe "console.warn($1)"
    js_consoleWarn :: JSString -> IO ()

foreign import javascript safe "console.log($1)"
    js_consoleLog :: JSString -> IO ()

foreign import javascript safe "console.log('%o', $1)"
    js_consoleLogObj :: JSRef a -> IO ()


foreign import javascript safe "$r = document;"
    js_document :: DocumentElement

foreign import javascript safe "$r = document.body;"
    js_documentBody :: DocumentElement


foreign import javascript safe "$r = document.getElementById($1);"
    js_getDocumentElement :: JSString -> IO DocumentElement


foreign import javascript unsafe "$r = $1.querySelector($2);"
    js_querySelector :: DocumentElement -> JSString -> IO DocumentElement


foreign import javascript unsafe "$r = $1.querySelectorAll($2);"
    js_querySelectorAll :: DocumentElement
                        -> JSString
                        -> IO DocumentElementCollection


foreign import javascript safe "$r = document.createElement($1)"
    js_documentCreateElement :: JSString -> IO DocumentElement


foreign import javascript safe "$1.classList.add($2);"
    js_classListAdd :: DocumentElement -> JSString -> IO ()

foreign import javascript safe "$1.classList.remove($2);"
    js_classListRemove :: DocumentElement -> JSString -> IO ()

foreign import javascript safe "$1.classList.contains($2)"
    js_classListContains :: DocumentElement -> JSString -> IO JSBool


foreign import javascript safe "$1.setAttribute($2,$3);"
    js_setAttribute :: DocumentElement -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$r = $1.getAttribute($2);"
    js_getAttribute :: DocumentElement -> JSString -> IO JSString


foreign import javascript safe "$1.removeAttribute($2);"
    js_removeAttribute :: DocumentElement -> JSString -> IO ()


foreign import javascript safe "$r = $1.innerText"
    js_getInnerText :: DocumentElement -> IO JSString

foreign import javascript safe "$1.innerText = $2"
    js_setInnerText :: DocumentElement -> JSString -> IO ()

foreign import javascript safe "$1.innerHTML = $2;"
    js_setInnerHtml:: DocumentElement -> JSString -> IO ()


-- TODO Продумать как можно использовать оптимальный вариант для данного
--      обозревателя (свойство dataset)
foreign import javascript safe "$1.setAttribute('data-' + $2, $3)"
    js_setDataAttribute :: DocumentElement -> JSString -> JSString -> IO ()

foreign import javascript safe "$1.removeAttribute('data-' + $2)"
    js_removeDataAttribute :: DocumentElement -> JSString -> IO ()

foreign import javascript safe "$r = $1.getAttribute('data-' + $2);"
    js_getDataAttribute :: DocumentElement -> JSString -> IO JSString


foreign import javascript safe "$r = $1.tagName;"
    js_tagName :: DocumentElement -> JSString


foreign import javascript safe "$2.appendChild($1)"
    js_appendChild :: DocumentElement
                   -- ^ элемент, который вставляется
                   -> DocumentElement
                   -- ^ старший элемент
                   -> IO ()

foreign import javascript safe "$2.insertBefore($1, $2.firstChild);"
    js_prependTo :: DocumentElement
                 -- ^ элемент, который вставляется
                 -> DocumentElement
                 -- ^ старший элемент
                 -> IO ()

foreign import javascript unsafe "$1.replaceChild($2,$3)"
  js_replaceChild :: DocumentElement
                  -- ^ родитель
                  -> DocumentElement
                  -- ^ новый элемент
                  -> DocumentElement
                  -- ^ заменяемый элемент
                  -> IO ()


foreign import javascript safe "$1.addEventListener($2,$3,$4);"
    js_addEventListener :: DocumentElement
                        -- ^ Элемент, чьи события слушать
                        -> JSString
                        -- ^ название события
                        -> JSFun (DocumentEvent -> IO ())
                        -- ^ функция, которая реагирует на события элемента
                        -> JSBool
                        -- ^ использовать захват события или нет?
                        -> IO ()

foreign import javascript safe "$1.dispatchEvent($2);"
    js_dispatchEvent :: DocumentElement -> JSRef a -> IO ()


foreign import javascript safe "$r = new Event($1);"
    js_newEvent :: JSString -> IO DocumentEvent

foreign import javascript safe "$r = function () {\
                               \    var d = {};\
                               \    d.detail = $2;\
                               \    var e = new CustomEvent($1, d);\
                               \    return e;\
                               \}();"
    js_newCustomEvent :: JSString -> JSRef a -> IO DocumentEvent

foreign import javascript safe "$1.stopPropagation();"
    js_eventStopPropagation :: DocumentEvent -> IO ()

foreign import javascript safe "$1.stopImmediatePropagation();"
    js_eventStopImmediatePropagation :: DocumentEvent -> IO ()

foreign import javascript safe "$1.oninput = $2;"
    js_setOnInput :: DocumentElement
               -> JSFun (IO ())
               -> IO ()


foreign import javascript safe "$r = window;"
    js_window :: JSRef a
foreign import javascript safe "$r = window;" js_windowSafe :: IO (JSRef a)

foreign import javascript safe "$1.preventDefault();"
    js_eventPreventDefault :: DocumentEvent -> IO ()

foreign import javascript safe "$1.remove();"
    js_remove :: OnElement

foreign import javascript safe "$r = $1.cloneNode($2);"
    js_cloneNode :: DocumentElement -> JSBool -> IO DocumentElement
--------------------------------------------------------------------------------
