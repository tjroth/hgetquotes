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
import Control.Monad

getLastQuote :: Symbol -> IO (Maybe YahooQuote)
getLastQuote sym = do
  cd <- fmap utctDay getCurrentTime
  mcs <- getCSV sym Daily cd 5
  case mcs of
   Just cs ->
     return $ headMay . toQuotes $ cs
   Nothing -> return Nothing

toLedgerFormat :: Symbol -> YahooQuote -> T.Text
toLedgerFormat sym q = T.concat ["P ", yQDate q,
                                      " 12:01:00 ",
                                      T.pack sym,
                                      T.pack " $",
                                      yQClose q]

ledgerQuote :: Bool -> Symbol -> IO (Maybe T.Text)
ledgerQuote verbose sym = do
  when verbose $
    putStrLn $ ("Attempting to downlad quote for " ++ sym) ++ "..."
  mQ <- getLastQuote sym
  case mQ of
    Just q ->when verbose $ putStrLn "Success!..."
    Nothing -> when verbose $ putStrLn "Download Failed"

  return $ fmap (toLedgerFormat sym) mQ
