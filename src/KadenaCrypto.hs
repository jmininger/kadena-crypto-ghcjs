{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module KadenaCrypto (generateRoot, generateKeypair, sign, verify) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Helper
import Language.Javascript.JSaddle.Types (ghcjsPure)
import GHCJS.Buffer
import GHCJS.Foreign
import Data.JSVal.Promise
import Control.Monad.IO.Class
import Data.Bits ((.|.))

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "lib.kadenaMnemonicToRootKeypair($1).then(function(value){ return value.buffer; })"
  kadenaMnemonicToRootKeypair :: Text -> JSVal

foreign import javascript unsafe "lib.kadenaGenKeypair($1, $2)"
  kadenaGenKeypair :: JSVal -> Int -> JSVal

foreign import javascript unsafe "lib.kadenaSign($1, $2)"
  kadenaSign :: JSVal -> JSVal -> JSVal

foreign import javascript unsafe "lib.kadenaVerify($1, $2, $3)"
  kadenaVerify :: JSVal -> JSVal -> JSVal -> Bool

arrayBufToByteString :: JSVal -> JSM ByteString
arrayBufToByteString jsBuf = do
  mutArrBuf <- mutableArrayBufferFromJSVal jsBuf
  mutBuf <- ghcjsPure $ createFromArrayBuffer mutArrBuf
  arrBuf <- freeze mutBuf
  pure $ toByteString 0 Nothing arrBuf

awaitPromise :: JSVal -> JSM (Maybe JSVal)
awaitPromise rawProm =
  fromJSVal rawProm >>= \case
    Nothing -> pure Nothing
    Just prom ->  flip fmap (liftIO $ await prom) $ \case
      Left _ -> Nothing
      Right val -> Just val

generateRoot :: Text -> JSM (Maybe ByteString)
generateRoot phrase = do
  mJSArr <- awaitPromise $ kadenaMnemonicToRootKeypair phrase 
  case mJSArr of
    Just buf -> Just <$> arrayBufToByteString buf
    Nothing -> pure Nothing

-- TODO: Test empty bs, test various int values
generateKeypair :: ByteString -> Int -> JSM (Maybe (ByteString, ByteString))
generateKeypair root idx = do
  rootBuf <- toJSVal $ BS.unpack root
  let idx' = fromIntegral $ 0x80000000 .|. idx
  res <- fromJSVal $ kadenaGenKeypair rootBuf idx' 
  case res of
    Nothing -> pure Nothing
    Just (prvJS, pubJS) -> fmap Just $ 
      (,) 
        <$> arrayBufToByteString prvJS
        <*> arrayBufToByteString pubJS

sign :: ByteString -> ByteString -> JSM (ByteString)
sign msg prv = do
  msgBuf <- toJSVal $ BS.unpack msg
  prvBuf <- toJSVal $ BS.unpack prv
  arrayBufToByteString $ kadenaSign msgBuf prvBuf

verify :: ByteString -> ByteString -> ByteString -> JSM Bool
verify msg pub sig = do
  msgBuf <- toJSVal $ BS.unpack msg
  pubBuf <- toJSVal $ BS.unpack pub
  sigBuf <- toJSVal $ BS.unpack sig
  return $ kadenaVerify msgBuf pubBuf sigBuf

#endif
