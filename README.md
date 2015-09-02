# hledger-getquotes

An add-on tool for the hledger double entry accounting package which allows you to easily download stock quotes.  hledger-getquotes downloads the most recent closing prices for all of the stock commodities included in your hledger file.

example: hledger getquotes -f ~/.hjournal.ledger -o ~/.prices.db

The above command will download all of the most recent closing prices for any stocks included in the ~/.hjournal.ledger file and will save them to the ~/.prices.db file.  The saved price format can also be used with ledger-cli.
