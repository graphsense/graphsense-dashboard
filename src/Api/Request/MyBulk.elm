module Api.Request.MyBulk exposing (Operation(..), bulkJson)

import Api
import Json.Decode
import Json.Encode


type Operation
    = OperationGetBlock
    | OperationListBlockTxs
    | OperationGetAddress
    | OperationListAddressTxs
    | OperationListTagsByAddress
    | OperationListAddressNeighbors
    | OperationGetAddressCluster
    | OperationListAddressLinks
    | OperationGetTx
    | OperationGetTxIo
    | OperationGetExchangeRates
    | OperationGetAddressTagSummary


stringFromOperation : Operation -> String
stringFromOperation model =
    case model of
        OperationGetBlock ->
            "get_block"

        OperationListBlockTxs ->
            "list_block_txs"

        OperationGetAddress ->
            "get_address"

        OperationListAddressTxs ->
            "list_address_txs"

        OperationListTagsByAddress ->
            "list_tags_by_address"

        OperationListAddressNeighbors ->
            "list_address_neighbors"

        OperationGetAddressCluster ->
            "get_address_cluster"

        OperationListAddressLinks ->
            "list_address_links"

        OperationGetTx ->
            "get_tx"

        OperationGetTxIo ->
            "get_tx_io"

        OperationGetExchangeRates ->
            "get_exchange_rates"

        OperationGetAddressTagSummary ->
            "get_tag_summary_by_address"


bulkJson : String -> Operation -> Json.Encode.Value -> Json.Decode.Decoder a -> Api.Request a
bulkJson currency_path operation_path body_body decoder =
    Api.request
        "POST"
        "/{currency}/bulk.json/{operation}"
        [ ( "currency", currency_path )
        , ( "operation", stringFromOperation operation_path )
        ]
        [ ( "num_pages", Just "1" ) ]
        []
        (Just body_body)
        decoder
