module Util.ExternalLinks exposing (addProtocolPrefx, getBlockExplorerLinks, getBlockExplorerTransactionLinks, getFontAwesomeIconForUris)

import Dict exposing (Dict)
import FontAwesome
import List.Extra
import Model.Tx exposing (AccountTxType(..), getTxHash, parseTxIdentifier)
import Regex exposing (Regex)


blockExplorerLinks : Dict String (List ( String, String ))
blockExplorerLinks =
    Dict.fromList
        [ ( "trx"
          , [ ( "https://tronscan.org/#/address/", "Tronscan" )
            ]
          )
        , ( "eth"
          , [ ( "https://etherscan.io/address/", "Etherscan" )
            , ( "https://www.oklink.com/eth/address/", "Oklink" )
            , ( "https://www.blockchain.com/eth/address/", "Blockchain.com" )
            , ( "https://blockchair.com/ethereum/address/", "Blockchair" )
            , ( "https://library.dedaub.com/contracts/Ethereum/", "Dedaub" )

            -- , ( "https://oko.palkeo.com/", "Palkeo" )
            ]
          )
        , ( "btc"
          , [ ( "https://www.oklink.com/btc/address/", "Oklink" )
            , ( "https://www.blockchain.com/btc/address/", "Blockchain.com" )
            , ( "https://blockchair.com/bitcoin/address/", "Blockchair" )
            ]
          )
        , ( "zec"
          , [ ( "https://blockchair.com/zcash/address/", "Blockchair" )
            ]
          )
        , ( "ltc"
          , [ ( "https://www.oklink.com/ltc/address/", "Oklink" )
            , ( "https://blockchair.com/litecoin/address/", "Blockchair" )
            ]
          )
        , ( "bch"
          , [ ( "https://www.oklink.com/bch/address/", "Oklink" )
            , ( "https://www.blockchain.com/bch/address/", "Blockchain.com" )
            , ( "https://blockchair.com/bitcoin-cash/address/", "Blockchair" )
            ]
          )
        ]


blockExplorerTransactionLinks : Dict String (List ( String, String ))
blockExplorerTransactionLinks =
    Dict.fromList
        [ ( "eth"
          , [ ( "https://etherscan.io/tx/0x", "Etherscan" )
            , ( "https://www.oklink.com/eth/tx/", "Oklink" )
            , ( "https://www.blockchain.com/eth/tx/", "Blockchain.com" )
            , ( "https://blockchair.com/ethereum/transaction/", "Blockchair" )
            ]
          )
        , ( "trx"
          , [ ( "https://tronscan.org/#/transaction/", "Tronscan" )
            ]
          )
        , ( "btc"
          , [ ( "https://www.oklink.com/btc/tx/", "Oklink" )
            , ( "https://www.blockchain.com/btc/tx/", "Blockchain.com" )
            , ( "https://blockchair.com/bitcoin/transaction/", "Blockchair" )
            ]
          )
        , ( "zec"
          , [ ( "https://blockchair.com/zcash/transaction/", "Blockchair" )
            ]
          )
        , ( "ltc"
          , [ ( "https://www.oklink.com/ltc/tx/", "Oklink" )
            , ( "https://blockchair.com/litecoin/transaction/", "Blockchair" )
            ]
          )
        , ( "bch"
          , [ ( "https://www.oklink.com/bch/tx/", "Oklink" )
            , ( "https://www.blockchain.com/bch/tx/", "Blockchain.com" )
            , ( "https://blockchair.com/bitcoin-cash/transaction/", "Blockchair" )
            ]
          )
        ]


getBlockExplorerTransactionLinksAnchors : String -> String -> AccountTxType -> String
getBlockExplorerTransactionLinksAnchors network url at =
    case ( network, at ) of
        ( "eth", Internal _ _ ) ->
            if String.contains "etherscan" url then
                "/advanced#internal"

            else
                ""

        ( "eth", Token _ tid ) ->
            if String.contains "etherscan" url then
                "/advanced#eventlog#" ++ (String.fromInt <| tid)

            else if String.contains "oklink" url then
                "/log"

            else if String.contains "tronscan" url then
                "/event-logs"

            else
                ""

        _ ->
            ""


getBlockExplorerLinks : String -> String -> List ( String, String )
getBlockExplorerLinks network address =
    blockExplorerLinks
        |> Dict.get network
        |> Maybe.withDefault []
        |> List.map (\( url_template, label ) -> ( url_template ++ address, label ))


getBlockExplorerTransactionLinks : String -> String -> List ( String, String )
getBlockExplorerTransactionLinks network txHash =
    let
        txIdM =
            parseTxIdentifier txHash
    in
    case txIdM of
        Just txId ->
            blockExplorerTransactionLinks
                |> Dict.get network
                |> Maybe.withDefault []
                |> List.map (\( url_template, label ) -> ( url_template ++ (txId |> getTxHash) ++ getBlockExplorerTransactionLinksAnchors network url_template txId, label ))

        _ ->
            []


fontAwesomeIconAssignments : List ( Regex, FontAwesome.Icon )
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
