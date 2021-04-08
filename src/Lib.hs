{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( app,
  )
where

import Data.Aeson (eitherDecode, encode)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as BSL (concat, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as C8 (pack)
import Data.Text (Text)
import qualified Data.Text as T (concat, replace)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO (readFile)
import Deriving.Aeson
import Network.HTTP.Types (parseSimpleQuery, status200, status400, status404)
import Network.Wai (Application, Request (..), rawPathInfo, rawQueryString, requestMethod, responseFile, responseLBS, strictRequestBody)

data CovidRequest = CovidRequest
  { creq'name :: Text,
    creq'tel :: Text,
    creq'covid19 :: Bool
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via StripApoSnake CovidRequest

data CovidResponse = CovidResponse
  { cresp'status :: Text,
    cresp'name :: Text,
    cresp'tel :: Text,
    cresp'covid19 :: Bool
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via StripApoSnake CovidResponse

covidFlow :: CovidRequest -> CovidResponse
covidFlow CovidRequest {..} =
  CovidResponse
    { cresp'status = "ok",
      cresp'name = creq'name,
      cresp'tel = creq'tel,
      cresp'covid19 = creq'covid19
    }

app :: FilePath -> Application
app dataDir request respond = do
  receiptTemplate <- TIO.readFile (dataDir ++ "/data/html/covid19-receipt.html")
  body <- strictRequestBody request
  respond $
    case (requestMethod request, rawPathInfo request, parseSimpleQuery (rawQueryString request)) of
      --    ("GET", "/covid19.html")
      ("GET", "/covid19.html", []) ->
        responseFile
          status200
          [("Content-Type", "text/html")]
          (dataDir ++ "/data/html/covid19-form.html")
          Nothing
      ("GET", "/covid19.html", q) -> do
        responseLBS
          status200
          [("Content-Type", "text/html")]
          $ BSL.fromStrict $ encodeUtf8 $ foldl replaceToken receiptTemplate $ map (bimap decodeUtf8 decodeUtf8) q
      ("POST", "/api/covid19", _) -> handleApi body
      _ -> notFound
  where
    handleApi body =
      case req' of
        Right req ->
          responseLBS
            status200
            [("Content-Type", "application/json")]
            (encode $ covidFlow req)
        Left err -> responseLBS status400 [("Content-Type", "application/json")] $ BSL.concat ["Unable to parse JSON:", C8.pack err]
      where
        req' = eitherDecode body
    replaceToken :: Text -> (Text, Text) -> Text
    replaceToken acc (k, v) = T.replace (T.concat ["<!-- $", k, " -->"]) v acc
    notFound =
      responseLBS
        status404
        [("Content-Type", "text/plain")]
        "404 - Not Found"

-- the machinery that makes the deriving via magic above work

type StripApoSnake = CustomJSON '[OmitNothingFields, FieldLabelModifier (StripToApostrophe, CamelToSnake)]

data StripToApostrophe

instance StringModifier StripToApostrophe where
  getStringModifier = stripToApostrophe

stripToApostrophe :: String -> String
stripToApostrophe = reverse . takeWhile (/= '\'') . reverse
