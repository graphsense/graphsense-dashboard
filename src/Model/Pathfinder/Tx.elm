module Model.Pathfinder.Tx exposing (..)

import Api.Data
import Dict exposing (Dict)
import Dict.Nonempty as NDict exposing (NonemptyDict)
import List.Nonempty as NList
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Input exposing (Input)
import Model.Pathfinder.Output exposing (Output)
import Tuple exposing (first)


type alias Tx =
    { id : Id
    , type_ : TxType
    }


type TxType
    = Account AccontTx
    | Utxo UtxoTx


type alias AccontTx =
    { from : Id
    , to : Id
    , value : Api.Data.Values
    }


type alias UtxoTx =
    { inputs : NonemptyDict Id Input
    , outputs : NonemptyDict Id Output
    }


hasAddress : Id -> Tx -> Bool
hasAddress id tx =
    hasOutput id tx || hasInput id tx


hasOutput : Id -> Tx -> Bool
hasOutput id tx =
    case tx.type_ of
        Account { to } ->
            to == id

        Utxo { outputs } ->
            NDict.get id outputs /= Nothing


hasInput : Id -> Tx -> Bool
hasInput id tx =
    case tx.type_ of
        Account { from } ->
            from == id

        Utxo { inputs } ->
            NDict.get id inputs /= Nothing


calcCoords : Dict Id Address -> Tx -> Result Error Coords
calcCoords addresses tx =
    (case tx.type_ of
        Account { from, to } ->
            NList.singleton from
                |> NList.append (NList.singleton to)

        Utxo { inputs, outputs } ->
            (NDict.toNonemptyList inputs |> NList.map first)
                |> NList.append (NDict.toNonemptyList outputs |> NList.map first)
    )
        |> (\list ->
                List.foldl
                    (\address ->
                        Result.andThen
                            (\r ->
                                getAddress addresses address
                                    |> Result.map NList.singleton
                                    |> Result.map (NList.append r)
                            )
                    )
                    (NList.head list
                        |> getAddress addresses
                        |> Result.map NList.singleton
                    )
                    (NList.tail list)
           )
        |> Result.map
            (NList.map addressToCoords
                >> Coords.avg
            )


avg : (Address -> Float) -> List Address -> Result Error Float
avg field items =
    if List.isEmpty items then
        items
            |> List.map (.id >> AddressNotFoundInDict >> InternalError)
            |> Errors
            |> Err

    else
        let
            its =
                List.map field items
        in
        List.sum its
            / toFloat (List.length its)
            |> Ok


getAddress : Dict Id Address -> Id -> Result Error Address
getAddress addresses id =
    Dict.get id addresses
        |> Maybe.map Ok
        |> Maybe.withDefault (AddressNotFoundInDict id |> InternalError |> Err)


addressToCoords : Address -> Coords
addressToCoords { x, y } =
    Coords x y
