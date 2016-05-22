# hledger-getquotes

## General

An add-on tool for the hledger program which allows one to easily download stock quotes using the Yahoo historical prices API.  The hledger-getquotes command downloads the most recent closing prices for all of the stocks included in your hledger file.

#### Example:
    >> hledger getquotes -- -f ~/ledger.journal -o ~/prices.db

The above command will download all of the most recent closing prices for any stocks included in the ~/ledger.journal file and will save them to the ~/prices.db file.  The prices will be saved with the following format:

P 2015/08/28 12:01:00 FB $87.230003

#### Example:
    >> hledger getquotes -- -f ~/ledger.journal -o ~/prices.db -d 2016-01-01

The above command will download all of the closing prices from 2016-01-01 to the current date.

#### Example:
    >> hledger -f ~/ledger.journal bal --price-db ~/prices.db -V

The above command will show the balance of all accounts with stock assets reported by currency value rather than by number of shares.

## Installation

#### Using Stack

    >> git clone git@github.com:tjroth/hgetquotes.git

    >> cd hgetquotes

    >> stack install

The hledger-getquotes program will be installed in ~/.local/bin.  You should add ~/.local/bin to your $PATH
