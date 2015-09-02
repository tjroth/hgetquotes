{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Control.Monad (mapM_)
import Options.Applicative
--import Control.Applicative
import Hledger
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)
import Data.List (isPrefixOf, nub)

data Operation = Operation
  { oHFile :: FilePath
  , oOFile :: FilePath }

runOp :: Parser Operation
runOp = Operation <$>
     strOption
         ( long "file"
           <> short 'f'
           <> metavar "HLEDGER FILE"
           <> value "~/.hledger.journal"
           <> help "hledger file" )
     <*> strOption
         ( long "out"
           <> short 'o'
           <> value "~/.hquotes.db"
           <> metavar "QUOTES-DB FILE"
           <> help "File to write quotes" )
   
         
getQuotesOp :: Operation -> IO ()
getQuotesOp (Operation f o ) = do
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
      --let ss = words $ args !! 1
      es <- mapM ledgerQuote syms
      if "~" `isPrefixOf` o
        then  mapM_ (quoteToFile (joinPath [homeDir, tail $ tail o])) es
        else  mapM_ (quoteToFile o) es
     


quoteToFile :: FilePath -> (Maybe T.Text) -> IO ()
quoteToFile f q = case q of
  Just q -> T.appendFile f (T.append "\n" q)
  Nothing -> return ()

main :: IO ()
main = execParser opts >>= getQuotesOp
  where
    opts = info (helper <*> runOp)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

{--
main :: IO ()
main = do
  args <- getArgs
  let f = head args
  let ss = words $ args !! 1
  es <- mapM ledgerQuote ss
  mapM_ (quoteToFile f) es


quoteToFile :: FilePath -> (Maybe T.Text) -> IO ()
quoteToFile f q = case q of
  Just q -> T.appendFile f (T.append "\n" q)
  Nothing -> return ()
  --}
                  
