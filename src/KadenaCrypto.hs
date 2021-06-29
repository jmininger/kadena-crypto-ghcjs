{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module KadenaCrypto where

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



-- import JavaScript.TypedArray
-- import JavaScript.TypedArray.ArrayBuffer

#ifdef ghcjs_HOST_OS

foreign import javascript safe "lib.kadenaMnemonicToRootKeypair($1).then(function(value){ return value.buffer; })"
  kadenaMnemonicToRootKeypair :: Text -> JSVal

foreign import javascript safe "lib.kadenaGenKeypair($1, $2)"
  kadenaGenKeypair :: JSVal -> Int -> JSVal -- (JSVal, JSVal) -- [JSBuf, JSBuf]

-- foreign import javascript safe "lib.kadenaSign($1, $2)"
--   kadenaSign :: Buffer -> Buffer -> JSVal -- JSBuf

-- foreign import javascript unsafe "lib.kadenaVerify($1, $2, $3)"
--   kadenaVerify :: Buffer -> Buffer -> Buffer -> Bool -- Bool
#endif

bsToBuffer :: ByteString -> JSM Buffer
bsToBuffer bs = do
  (buf, _, _) <- ghcjsPure $ fromByteString bs
  pure buf

arrayBufToByteString :: JSVal -> JSM ByteString
arrayBufToByteString jsBuf = do
  mutArrBuf <- mutableArrayBufferFromJSVal jsBuf
  mutBuf <- ghcjsPure $ createFromArrayBuffer mutArrBuf
  arrBuf <- freeze mutBuf
  pure $ toByteString 0 Nothing arrBuf

generateRoot :: Text -> JSM (Maybe ByteString)
generateRoot phrase = do
  mProm <- fromJSVal $ kadenaMnemonicToRootKeypair phrase
  case mProm of
    Nothing -> pure Nothing
    Just prom -> do
      promRes <- liftIO $ await prom
      case promRes of
        Left _ -> pure Nothing
        Right buf -> 
          Just <$> arrayBufToByteString buf

-- TODO: Test empty bs, test various int values
generateKeypair :: ByteString -> Int -> JSM (ByteString, ByteString)
generateKeypair root idx = do
  rootBuf <- toJSVal $ BS.unpack root
  let idx' = fromIntegral $ 0x80000000 .|. idx
  res <- fromJSVal $ kadenaGenKeypair rootBuf idx' 
  case res of
    Nothing -> liftIO $ putStrLn "Could Not unpack pair" >> pure ("", "")
    Just (prvJS, pubJS) -> do
      prv <- arrayBufToByteString prvJS
      pub <- arrayBufToByteString pubJS
      pure (prv, pub)




-- readPromise ... from JSDom.Types
