{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Marshalling where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import Control.Applicative

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

type Annotation = String

newtype Host = Host String deriving (Eq, Show)
instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

data Color = Red Annotation | Blue Annotation | Yellow Annotation deriving (Eq, Show)
instance FromJSON Color where
  parseJSON (Object v) = 
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yello")
  parseJSON _ = fail "Expected an object for Color"

data TestData = TestData { section :: Host, what :: Color } deriving (Eq, Show)
instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for Test Data"

main = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d
