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
          , [ ( "https://tronscan.org/#/address/", "Open Tronscan" )
            ]
          )
        , ( "eth"
          , [ ( "https://etherscan.io/address/", "Open Etherscan" )
            , ( "https://www.oklink.com/eth/address/", "Open Oklink" )
            , ( "https://www.blockchain.com/eth/address/", "Open Blockchain.com" )
            , ( "https://blockchair.com/ethereum/address/", "Open Blockchair" )
            , ( "https://library.dedaub.com/contracts/Ethereum/", "Open Dedaub" )

            -- , ( "https://oko.palkeo.com/", "Open Palkeo" )
            ]
          )
        , ( "btc"
          , [ ( "https://www.oklink.com/btc/address/", "Open Oklink" )
            , ( "https://www.blockchain.com/btc/address/", "Open Blockchain.com" )
            , ( "https://blockchair.com/bitcoin/address/", "Open Blockchair" )
            ]
          )
        , ( "zec"
          , [ ( "https://blockchair.com/zcash/address/", "Open Blockchair" )
            ]
          )
        , ( "ltc"
          , [ ( "https://www.oklink.com/ltc/address/", "Open Oklink" )
            , ( "https://blockchair.com/litecoin/address/", "Open Blockchair" )
            ]
          )
        , ( "bch"
          , [ ( "https://www.oklink.com/bch/address/", "Open Oklink" )
            , ( "https://www.blockchain.com/bch/address/", "Open Blockchain.com" )
            , ( "https://blockchair.com/bitcoin-cash/address/", "Open Blockchair" )
            ]
          )
        ]


blockExplorerTransactionLinks : Dict String (List ( String, String ))
blockExplorerTransactionLinks =
    Dict.fromList
        [ ( "eth"
          , [ ( "https://etherscan.io/tx/0x", "Open Etherscan" )
            , ( "https://www.oklink.com/eth/tx/", "Open Oklink" )
            , ( "https://www.blockchain.com/eth/tx/", "Open Blockchain.com" )
            , ( "https://blockchair.com/ethereum/transaction/", "Open Blockchair" )
            ]
          )
        , ( "trx"
          , [ ( "https://tronscan.org/#/transaction/", "Tronscan" )
            ]
          )
        , ( "btc"
          , [ ( "https://www.oklink.com/btc/tx/", "Open Oklink" )
            , ( "https://www.blockchain.com/btc/tx/", "Open Blockchain.com" )
            , ( "https://blockchair.com/bitcoin/transaction/", "Open Blockchair" )
            ]
          )
        , ( "zec"
          , [ ( "https://blockchair.com/zcash/transaction/", "Open Blockchair" )
            ]
          )
        , ( "ltc"
          , [ ( "https://www.oklink.com/ltc/tx/", "Open Oklink" )
            , ( "https://blockchair.com/litecoin/transaction/", "Open Blockchair" )
            ]
          )
        , ( "bch"
          , [ ( "https://www.oklink.com/bch/tx/", "Open Oklink" )
            , ( "https://www.blockchain.com/bch/tx/", "Open Blockchain.com" )
            , ( "https://blockchair.com/bitcoin-cash/transaction/", "Open Blockchair" )
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
