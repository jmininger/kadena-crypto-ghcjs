{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import KadenaCrypto
import qualified Data.ByteString.Base16 as B16
import Language.Javascript.JSaddle
import Control.Monad.IO.Class

main :: IO ()
main = flip runJSM () $ do
  let phrase = "welcome coffee sketch silver cube bundle wink endless six grief town learn"
  Just root <- generateRoot phrase
  print $ B16.encode root
  Just (prv, pub) <- generateKeypair root 0
  liftIO $ print $ B16.encode prv
  liftIO $ print $ B16.encode pub

  Just (prv2, pub2) <- generateKeypair root 1
  let msg1 = "hello world"
  let msg2 = "not hello world"
  sig1 <- sign msg1 prv
  sig2 <- sign msg2 prv
  sig3 <- sign msg1 prv2

  liftIO . print =<< verify msg1 pub sig1  -- Expect: True
  liftIO . print =<< verify msg1 pub sig2  -- Expect: False
  liftIO . print =<< verify msg1 pub sig3  -- Expect: False
  liftIO . print =<< verify msg1 pub2 sig3  -- Expect: True
