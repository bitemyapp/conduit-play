{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONSpec (main, spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import Text.RawString.QQ
import Test.Hspec

import JSON


exampleJSON = [r|
{
    "version": 1,
    "root": {
      "type": "directory",
      "entries": {
        "lib": {
          "type": "directory",
          "entries": {
            "Mcrt1.o": {
              "type": "regular",
              "size": 1288
            },
            "Scrt1.o": {
              "type": "regular",
              "size": 3920
            }
          }
        }
      }
    }
  }
|]

examplePath =
  Path [NewPath "lib" (Path [
    NewPath "Mcrt1.o" (FilePath "Mcrt1.o" (File 1288 False)),
    NewPath "Scrt1.o" (FilePath "Scrt1.o" (File 3920 False))])]

spec :: Spec
spec = do
  describe "parsing" $ do
    it "exampleJSON" $ do
      (eitherDecode exampleJSON :: Either String Path) `shouldBe` Right examplePath


main :: IO ()
main = hspec spec
