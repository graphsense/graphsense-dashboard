module Mockup.Search exposing (..)

import Api.Data


abcd : Api.Data.SearchResult
abcd =
    { currencies =
        [ { currency = "btc"
          , addresses =
                [ "abcdefg"
                , "abcdxyz"
                ]
          , txs = []
          }
        ]
    , labels = []
    }
