{-# LANGUAGE OverloadedStrings #-}

module YahooAPI where

import qualified Control.Exception    as E
import           Control.Lens
import           Control.Monad        (mzero)
import           Data.ByteString.Lazy
import           Data.Csv
import           Data.Text
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Calendar
import           Data.Typeable
import qualified Data.Vector          as V
import qualified Network.HTTP.Client  as HC
import           Network.Wreq

data YahooQuote = YahooQuote { yQDate     :: T.Text,
                               yQOpen     :: T.Text,
                               yQHigh     :: T.Text,
                               yQLow      :: T.Text,
                               yQClose    :: T.Text,
                               yQVolume   :: T.Text,
                               yQAdjClose :: T.Text } deriving (Show, Eq)

type Symbol = String

data Interval = Daily | Weekly | Monthly deriving (Eq, Ord)

instance Show Interval where
  show Daily = "d"
  show Weekly = "w"
  show Monthly = "m"

--data NetworkException = InvalidSymbol String deriving (Typeable)

--instance E.Exception NetworkException

{-}
instance Show NetworkException where
  show (InvalidSymbol s) = "EXCEPTION: Invalid URL or ticker symbol \'"  ++ s ++ "\'"
-}

instance FromRecord YahooQuote where
  parseRecord v
    | V.length v == 7 = YahooQuote  <$> v .! 0
                                    <*> v .! 1
                                    <*> v .! 2
                                    <*> v .! 3
                                    <*> v .! 4
                                    <*> v .! 5
                                    <*> v .! 6
    | otherwise = mzero


toQuotes :: ByteString -> [YahooQuote]
toQuotes qbs =
  let
    vresult = (decode HasHeader qbs :: Either String (V.Vector YahooQuote))
  in
   case vresult of
     Left e -> []
     Right v ->  V.toList v


getCSV :: Symbol -> Interval -> Day -> Integer -> IO (Maybe ByteString)
getCSV sym intvl endday numdays = do
  r <- E.try (getWith opts baseUrl) :: IO (Either E.SomeException (Response ByteString)) -- `E.catch` handler
  case r of
   Left ex -> return Nothing
   Right rbs -> return $ Just $ rbs ^. responseBody
  where
      handler e@(HC.StatusCodeException s _ _)
        | s ^. statusCode == 404 = return Nothing :: IO (Maybe ByteString) --E.throwIO (InvalidSymbol sym)
        | otherwise = E.throwIO e
      baseUrl = "http://real-chart.finance.yahoo.com/table.csv"
      stday = addDays (-(numdays-1)) endday
      (f,d,e) = toGregorian endday
      (c,a,b) = toGregorian stday
      opts = defaults &
           param "a" .~ [T.pack . show $ a-1] &
           param "b" .~ [T.pack $ show b] &
           param "c" .~ [T.pack $ show c] &
           param "d" .~ [T.pack . show $ d-1] &
           param "e" .~ [T.pack $ show e] &
           param "f" .~ [T.pack $ show f] &
           param "ignore" .~ [".csv"] &
           param "s" .~ [T.pack sym] &
           param "g" .~ [T.pack $ show intvl]
