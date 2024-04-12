module Update.Pathfinder.Network exposing (..)

import Api.Data
import Dict
import Effect exposing (n)
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Address as Address
import List.Nonempty as NList
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx
import Plugin.Update as Plugin exposing (Plugins)
import Result.Extra


addAddress : Plugins -> Id -> Api.Data.Address -> Network -> ( Network, List Effect )
addAddress plugins id data model =
    case Dict.get id model.addresses of
        Nothing ->
            loadAddress plugins id data model

        Just _ ->
            n model


loadAddress : Plugins -> Id -> Api.Data.Address -> Network -> ( Network, List Effect )
loadAddress plugins id data model =
    let
        newAddress =
            findAddressPosition id model
                |> Result.map (Address.init id data)
    in
    case newAddress of
        Ok na ->
            { model
                | addresses = Dict.insert id na model.addresses
            }
                |> n

        Err err ->
            ( model
            , [ ErrorEffect err ]
            )


findAddressPosition : Id -> Network -> Result Error Coords
findAddressPosition id network =
    listTxsForAddress network id
        |> List.map (Tx.calcCoords network.addresses)
        |> Result.Extra.combine
        |> Result.map
            (NList.fromList
                >> Maybe.map Coords.avg
                >> Maybe.withDefault { x = 0, y = 0 }
            )
