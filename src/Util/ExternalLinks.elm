module Util.ExternalLinks exposing (..)

import Dict


blockExplorerLinks : Dict.Dict String (List ( String, String ))
blockExplorerLinks =
    Dict.fromList
        [ ( "eth"
          , [ ( "https://etherscan.io/address/", "Open on Etherscan" )
            , ( "https://www.oklink.com/eth/address/", "Open in Oklink" )
            , ( "https://www.blockchain.com/eth/address/", "Open on Blockchain.com" )
            , ( "https://blockchair.com/ethereum/address/", "Open in blockchair" )
            , ( "https://library.dedaub.com/contracts/Ethereum/", "Open on dedaub" )
            , ( "https://oko.palkeo.com/", "Open on palkeo" )
            ]
          )
        , ( "btc"
          , [ ( "https://www.oklink.com/btc/address/", "Open in Oklink" )
            , ( "https://www.blockchain.com/btc/address/", "Open on Blockchain.com" )
            , ( "https://blockchair.com/bitcoin/address/", "Open in blockchair" )
            ]
          )
        , ( "zec"
          , [ ( "https://blockchair.com/zcash/address/", "Open in blockchair" )
            ]
          )
        , ( "ltc"
          , [ ( "https://www.oklink.com/ltc/address/", "Open in Oklink" )
            , ( "https://blockchair.com/litecoin/address/", "Open in blockchair" )
            ]
          )
        , ( "bch"
          , [ ( "https://www.oklink.com/bch/address/", "Open in Oklink" )
            , ( "https://www.blockchain.com/bch/address/", "Open on Blockchain.com" )
            , ( "https://blockchair.com/bitcoin-cash/address/", "Open in blockchair" )
            ]
          )
        ]


getBlockExplorerLinks : String -> String -> List ( String, String )
getBlockExplorerLinks currency address =
    blockExplorerLinks
        |> Dict.get currency
        |> Maybe.withDefault []
        |> List.map (\( url_template, label ) -> ( url_template ++ address, label ))
