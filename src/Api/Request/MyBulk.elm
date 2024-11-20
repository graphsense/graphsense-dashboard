module Api.Request.MyBulk exposing (Operation(..), bulkJson, operationVariants, stringFromOperation)

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
    | OperationGetAddressEntity
    | OperationListAddressLinks
    | OperationGetEntity
    | OperationListTagsByEntity
    | OperationListEntityNeighbors
    | OperationListEntityTxs
    | OperationListEntityLinks
    | OperationListEntityAddresses
    | OperationGetTx
    | OperationGetTxIo
    | OperationGetExchangeRates


operationVariants : List Operation
operationVariants =
    [ OperationGetBlock
    , OperationListBlockTxs
    , OperationGetAddress
    , OperationListAddressTxs
    , OperationListTagsByAddress
    , OperationListAddressNeighbors
    , OperationGetAddressEntity
    , OperationListAddressLinks
    , OperationGetEntity
    , OperationListTagsByEntity
    , OperationListEntityNeighbors
    , OperationListEntityTxs
    , OperationListEntityLinks
    , OperationListEntityAddresses
    , OperationGetTx
    , OperationGetTxIo
    , OperationGetExchangeRates
    ]


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

        OperationGetAddressEntity ->
            "get_address_entity"

        OperationListAddressLinks ->
            "list_address_links"

        OperationGetEntity ->
            "get_entity"

        OperationListTagsByEntity ->
            "list_tags_by_entity"

        OperationListEntityNeighbors ->
            "list_entity_neighbors"

        OperationListEntityTxs ->
            "list_entity_txs"

        OperationListEntityLinks ->
            "list_entity_links"

        OperationListEntityAddresses ->
            "list_entity_addresses"

        OperationGetTx ->
            "get_tx"

        OperationGetTxIo ->
            "get_tx_io"

        OperationGetExchangeRates ->
            "get_exchange_rates"


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
