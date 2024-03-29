module Mockup.Search exposing (..)

import Api.Data


abcd : Api.Data.SearchResult
abcd =
    { currencies =
        [ { currency = "btc"
          , addresses =
                [ "abcdefg123456"
                , "abcdxyz789012"
                ]
          , txs = []
          }
        ]
    , labels = []
    }
