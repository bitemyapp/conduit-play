{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson (eitherDecode)
import Control.Concurrent.ParallelIO.Local
import Control.Monad (when)
import Data.Conduit.Lzma as L
import Data.Conduit.List (consume)
import Data.Maybe (catMaybes)
import Data.Either (isLeft)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)

import JSON (Path)

-- Example: https://cache.nixos.org/626rn6vqqv1pbrlidwyb2ddppmb9hm38.ls.xz


-- "/nix/store/d4whi0j8sx07j8fyjb08d2xx4cf4h3p5-aspell-dict-nb-0.50.1-0"
--Left "Error in $: Failed reading: Cannot decode byte '\\x6c': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"

pathToIndex :: BS.ByteString -> BS.ByteString
pathToIndex path =
  "https://cache.nixos.org/" <> hash <> ".ls.xz"
    where
      hash = BS.take 32 $ BS.drop 11 path

retrieveXZ :: Manager -> String -> IO [BS.ByteString]
retrieveXZ manager url = do
  request <- parseRequest url
  runResourceT $ do
    response <- http request manager
    responseBody response C.$=+ L.decompress Nothing C.$$+- consume

transform :: Manager -> BS.ByteString -> IO (Either String Path)
transform manager url = do
  result <- fmap (eitherDecode . BSL.fromStrict) $ (fmap BS.concat) $ (retrieveXZ manager) $ BS.unpack $ pathToIndex $ url
  print url
  when (isLeft result) $ do
    print $ result
  return result

indexStorePaths :: String -> IO ()
indexStorePaths link = do
  -- TODO: HTTP2 would speed this up
  manager <- newManager tlsManagerSettings

  -- get store-paths.xz
  tempStorePaths <- retrieveXZ manager link

  -- get list of store paths
  let storePaths = BS.lines $ BS.concat $ tempStorePaths

  -- for each hash, request the ls.xz in new IO fork
  exceptions <- withPool 40 $ \pool ->
    parallelE_ pool $ fmap (transform manager) storePaths
    -- TODO: check if store path was already indexed
    --
    -- TODO: index JSON contents into SQL (see upstream schema)
    -- TODO: figure out system and attribute (from narinfo -> drv?)
    -- TODO: retry failed
  print $ catMaybes exceptions


-- TODO: remember if particular channel was already indexed
someFunc :: IO ()
someFunc = indexStorePaths "https://d3g5gsiof5omrk.cloudfront.net/nixos/16.09/nixos-16.09.975.1e1112e/store-paths.xz"
