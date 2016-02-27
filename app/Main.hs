{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad       (mapM_)
import           Data.List           (isPrefixOf, nub)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Hledger
import           Lib
import           Options.Applicative
import           System.Directory    (getHomeDirectory)
import           System.Environment
import           System.FilePath     (joinPath)

data Operation = Operation
  { oHFile :: FilePath
  , oOFile :: FilePath
  , oVerbose :: Bool }

runOp :: Parser Operation
runOp = Operation <$>
    strOption
      ( long "file"
        <> short 'f'
        <> metavar "HLEDGER FILE"
        <> value "~/.hledger.journal"
        <> help "hledger file, default ~/.hledger.journal" )
    <*> strOption
      ( long "out"
        <> short 'o'
        <> value "~/.hquotes.db"
        <> metavar "QUOTES-DB FILE"
        <> help "File to save quotes, default ~/.hquotes.db" )
    <*> switch
      ( long "verbose"
        <> short 'v'
        <> help "Enable verbose mode")


getQuotesOp :: Operation -> IO ()
getQuotesOp (Operation f o v) = do
  homeDir <- getHomeDirectory
  if "~" `isPrefixOf` f
    then readJ (joinPath [homeDir, tail $ tail f])
    else readJ f
  where
    readJ nf = do
      jf <- readJournalFile Nothing Nothing False nf
      case jf of
       Right j -> writeQuotesToFile o j
       Left e -> putStrLn $ "Could not read specified journal file " ++ f
    writeQuotesToFile o j = do
      homeDir <- getHomeDirectory
      let syms = nub $  map acommodity  $ filter (\a -> acommodity a /= "$" && acommodity a /= "") $ concat $ map amounts $ map pamount $ concat $ map tpostings $ jtxns j

      es <- mapM (ledgerQuote v) syms
      if "~" `isPrefixOf` o
        then  mapM_ (quoteToFile (joinPath [homeDir, tail $ tail o])) es
        else  mapM_ (quoteToFile o) es


quoteToFile :: FilePath -> Maybe T.Text -> IO ()
quoteToFile f q = case q of
  Just q -> T.appendFile f (T.append "\n" q)
  Nothing -> return ()


main :: IO ()
main = execParser opts >>= getQuotesOp
  where
    opts = info (helper <*> runOp)
      ( fullDesc
     <> progDesc "Download stock quotes for HLEDGER FILE and save to QUOTES-DB FILE"
     <> header "hledger-quotes - add-on tool for hledger to download stock quotes" )
