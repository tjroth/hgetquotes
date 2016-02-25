{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ledgerQuote,
      toLedgerFormat,
      getLastQuote
    ) where

import qualified Data.Text as T
import           Data.Time
import           Safe
import           YahooAPI

getLastQuote :: Symbol -> IO (Maybe YahooQuote)
getLastQuote sym = do
  cd <- fmap utctDay getCurrentTime
  mcs <- getCSV sym Daily cd 5
  case mcs of
   Just cs -> do
     return $ headMay . toQuotes $ cs
   Nothing -> return Nothing

toLedgerFormat :: Symbol -> YahooQuote -> T.Text
toLedgerFormat sym q = T.concat ["P ", (yQDate q),
                                    " 12:01:00 ",
                                    (T.pack sym),
                                    (T.pack " $"),
                                    (yQClose q)]

ledgerQuote :: Symbol -> IO (Maybe T.Text)
ledgerQuote sym = do
  mQ <- getLastQuote sym
  return $ fmap (toLedgerFormat sym) mQ
