{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Data.Conduit.Lzma as L
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)


someFunc :: IO ()
someFunc = do
  request <- parseRequest "https://cache.nixos.org/sb3r5grph8lyv9zvam2my3vwwag64fi7.ls.xz"
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    responseBody response C.$=+ L.decompress Nothing C.$$+- sinkFile "google.html"
