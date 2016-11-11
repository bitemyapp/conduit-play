{-# LANGUAGE OverloadedStrings #-}

module JSON
    ( Path (..)
    , File (..)
    , NewPath (..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable (for)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM


type Size = Integer
type IsExecutable = Bool
data File = File Size IsExecutable deriving (Show, Eq)

type Name = Text
type Type = Text
type Target = Text
data Path =
    Path [NewPath]
  | FilePath Name File 
  | Symlink Name Target
  deriving (Show, Eq)

data NewPath = NewPath Name Path deriving (Show, Eq)
-- (.:) :: FromJSON a => Object -> Text -> Parser a
-- parseJSON :: Value -> Parser a
-- toList :: HashMap k v -> [(k, v)]
-- data Value = Object Object | Array ...
-- type Object = HashMap Text Value

instance FromJSON Path where
  -- TODO: assert version is 1 and warn

  parseJSON = withObject "root" $ \o -> do
    root <- o .: "root"
    parseListing "root" root

parseListing :: Text -> Object -> Parser Path
parseListing key obj =
  case HM.lookup "type" obj of
    (Just "directory") -> do
        entries <- obj .: "entries"
        newpaths <- for (HM.toList entries) $ \(key, value) -> do
          path <- parseListing key value
          return $ NewPath key path
        return $ Path newpaths
    (Just "regular") -> do
        size <- (obj .: "size")
        isExecutable <- (obj .:? "executable" .!= False)
        return . FilePath key $ File size isExecutable
    (Just "symlink") -> do
        target <- (obj .: "target")
        return $ Symlink key target
    _ -> fail "Unknown type listing"
