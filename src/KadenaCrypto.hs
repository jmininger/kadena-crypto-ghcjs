{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module KadenaCrypto (genMnemonic, generateRoot, generateKeypair, sign, verify) where

import           Control.Monad.IO.Class
import           Data.Bits                          ((.|.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.Text                          (Text)
import           Data.Word                          (Word8)
import           GHCJS.Buffer                       (createFromArrayBuffer, freeze, toByteString)
import           Language.Javascript.JSaddle
import           Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import           Language.Javascript.JSaddle.Types  (ghcjsPure)

-- #ifdef ghcjs_HOST_OS
foreign import javascript unsafe "lib.kadenaGenMnemonic()"
  kadenaGenMnemonic :: JSM JSString

foreign import javascript unsafe "lib.kadenaMnemonicToRootKeypair($1)"
  kadenaMnemonicToRootKeypair :: Text -> JSVal

foreign import javascript unsafe "lib.kadenaGenKeypair($1, $2)"
  kadenaGenKeypair :: JSVal -> Int -> JSVal

foreign import javascript unsafe "lib.kadenaGetPublic($1)"
  kadenaGetPublic :: JSVal -> JSVal

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

generateRoot :: Text -> JSM (Maybe ByteString)
generateRoot phrase = do
  mJSArr <- fromJSVal $ kadenaMnemonicToRootKeypair phrase
  case mJSArr of
    Just buf -> Just <$> arrayBufToByteString buf
    Nothing  -> pure Nothing

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
verify msg pub sig = kadenaVerify
  <$> toBuf msg
  <*> toBuf pub
  <*> toBuf sig
  where toBuf = toJSVal . BS.unpack

genMnemonic :: JSM Text
genMnemonic =
  textFromJSString <$> kadenaGenMnemonic

-- #endif
