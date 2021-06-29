{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- import Data.Word (Word8)
import KadenaCrypto
import qualified Data.ByteString.Base16 as B16
import Language.Javascript.JSaddle
-- import Language.Javascript.JSaddle.Monad
import Control.Monad.IO.Class
-- import Language.Javascript.JSaddle.Types
-- import Data.JSVal.Promise




-- main :: IO ()
-- main = do
--   let phrase = "welcome coffee sketch silver cube bundle wink endless six grief town learn"
--   val <- generateRoot phrase
--   print $ B16.encode <$> val
main :: IO ()
main = flip runJSM () $ do
  let phrase = "welcome coffee sketch silver cube bundle wink endless six grief town learn"
  Just root <- generateRoot phrase
  print $ B16.encode root
  (prv, pub) <- generateKeypair root 0
  liftIO $ print $ B16.encode prv
  liftIO $ print $ B16.encode pub
