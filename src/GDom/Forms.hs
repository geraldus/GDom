module GDom.Forms (
  inputType
, inputTypeSafe
, inputTypeMT
) where

import           Control.Monad             (when)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Text                 (Text)
import           GDom.CommonDom            (tagName)
import           GDom.Types
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types

--------------------------------------------------------------------------------
inputType :: DocumentElement -> IO Text
inputType e = return . fromJSString =<< js_inputType e

inputTypeSafe :: DocumentElement -> IO (Maybe InputType)
inputTypeSafe e = do
    tnam <- tagName e
    when (tnam /= InputTagName)
            (error "inputTypeSafe shoould be used with input elements only.")
    ityp <- js_inputType e
    fromJSRef (castRef ityp)

inputTypeMT :: DocumentElement -> MaybeT IO InputType
inputTypeMT e = tNameCheck >>= getType
    where
        tNameCheck = do
            tname <- liftIO . tagName $ e
            if tname /= InputTagName
                then MaybeT . return $ Nothing
                else return ()
        getType _ = do
            typ <- liftIO . js_inputType $ e
            MaybeT . fromJSRef . castRef $ typ
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
foreign import javascript safe "$r = $1.type;"
    js_inputType :: DocumentElement -> IO JSString
--------------------------------------------------------------------------------
