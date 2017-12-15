{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ReqXRP where


import           Control.Exception     (throwIO)
import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO


data TickerResponse =
  TickerResponse
    { last      :: Double
    , high      :: Double
    , low       :: Double
    , vwap      :: Double
    , volume    :: Double
    , bid       :: Double
    , ask       :: Double
    , timestamp :: Integer
    , open      :: Double
    } deriving Show

instance FromJSON TickerResponse where
  parseJSON (Object v)
    =   TickerResponse
    <$> fmap read (v .: "last")
    <*> fmap read (v .: "high")
    <*> fmap read (v .: "low")
    <*> fmap read (v .: "vwap")
    <*> fmap read (v .: "volume")
    <*> fmap read (v .: "bid")
    <*> fmap read (v .: "ask")
    <*> fmap read (v .: "timestamp")
    <*> fmap read (v .: "open")


reqXRP :: IO (Maybe TickerResponse)
reqXRP = do
  response <-
    req
      GET
      (https "bitstamp.net" /: "api" /: "v2" /: "ticker" /: "xrpeur")
      NoReqBody
      lbsResponse
      mempty

  return (decode (responseBody response) :: Maybe TickerResponse)
