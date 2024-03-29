{-
   GraphSense API
   GraphSense API provides programmatic access to various ledgers' addresses, entities, blocks, transactions and tags for automated and highly efficient forensics tasks.

   The version of the OpenAPI document: 1.3.0
   Contact: contact@ikna.io

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git

   DO NOT EDIT THIS FILE MANUALLY.

   For more info on generating Elm code, see https://eriktim.github.io/openapi-elm/
-}


module Api.Request.Blocks exposing (..)

import Api
import Api.Data
import Json.Decode


getBlock : String -> Int -> Api.Request Api.Data.Block
getBlock currency_path height_path =
    Api.request
        "GET"
        "/{currency}/blocks/{height}"
        [ ( "currency", identity currency_path ), ( "height", String.fromInt height_path ) ]
        []
        []
        Nothing
        Api.Data.blockDecoder


listBlockTxs : String -> Int -> Api.Request (List Api.Data.Tx)
listBlockTxs currency_path height_path =
    Api.request
        "GET"
        "/{currency}/blocks/{height}/txs"
        [ ( "currency", identity currency_path ), ( "height", String.fromInt height_path ) ]
        []
        []
        Nothing
        (Json.Decode.list Api.Data.txDecoder)
