module Util.ExternalLinks exposing (..)

import Dict
import FontAwesome
import List.Extra
import Regex


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


fontAwesomeIconAssignments : List ( Regex.Regex, FontAwesome.Icon )
fontAwesomeIconAssignments =
    [ ( Regex.fromString "github.com" |> Maybe.withDefault Regex.never, FontAwesome.github )
    , ( Regex.fromString "twitter.com" |> Maybe.withDefault Regex.never, FontAwesome.twitter )
    , ( Regex.fromString "facebook.com" |> Maybe.withDefault Regex.never, FontAwesome.facebook )
    , ( Regex.fromString "linkedin.com" |> Maybe.withDefault Regex.never, FontAwesome.linkedin )
    , ( Regex.fromString "wikipedia.org" |> Maybe.withDefault Regex.never, FontAwesome.wikipediaW )
    , ( Regex.fromString "reddit.com" |> Maybe.withDefault Regex.never, FontAwesome.reddit )
    , ( Regex.fromString "instagram.com" |> Maybe.withDefault Regex.never, FontAwesome.instagram )
    ]


getFontAwesomeIconForUri : String -> Maybe FontAwesome.Icon
getFontAwesomeIconForUri uri =
    List.Extra.find (\( regex, _ ) -> Regex.contains regex uri) fontAwesomeIconAssignments |> Maybe.map Tuple.second


getFontAwesomeIconForUris : List String -> List ( String, Maybe FontAwesome.Icon )
getFontAwesomeIconForUris uris =
    List.map getFontAwesomeIconForUri uris |> List.map2 Tuple.pair uris


addProtocolPrefx : String -> String
addProtocolPrefx uri =
    if String.startsWith "http" uri then
        uri

    else
        "https://" ++ uri
