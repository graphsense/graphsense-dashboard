module Model.Pathfinder.Tx exposing (..)

import Api.Data
import Config.Pathfinder exposing (addressXOffset)
import Dict exposing (Dict)
import Dict.Nonempty as NDict exposing (NonemptyDict)
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (first)
import Util.Pathfinder exposing (getAddress)


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
    { x : Float
    , y : Float
    , inputs : NonemptyDict Id Api.Data.Values
    , outputs : NonemptyDict Id Api.Data.Values
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


getAddressesForTx : Dict Id Address -> Tx -> List Address
getAddressesForTx addresses tx =
    (case tx.type_ of
        Account { from, to } ->
            [ from, to ]

        Utxo { inputs, outputs } ->
            (NDict.toList inputs |> List.map first) ++ (NDict.toList outputs |> List.map first)
    )
        |> List.filterMap
            (getAddress addresses >> Result.toMaybe)


calcCoords : NList.Nonempty Address -> Coords
calcCoords =
    NList.map addressToCoords >> Coords.avg


getCoords : Tx -> Maybe Coords
getCoords tx =
    case tx.type_ of
        Utxo { x, y } ->
            Coords x y
                |> Just

        Account _ ->
            Nothing


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


addressToCoords : Address -> Coords
addressToCoords { x, y } =
    Coords x y


fromData : Api.Data.Tx -> Direction -> Coords -> Result Error Tx
fromData data direction anchor =
    case data of
        Api.Data.TxTxAccount t ->
            let
                id =
                    Id.init t.currency t.txHash
            in
            Ok
                { id = id
                , type_ =
                    Account
                        { from = Id.init t.currency t.fromAddress
                        , to = Id.init t.currency t.toAddress
                        , value = t.value
                        }
                }

        Api.Data.TxTxUtxo t ->
            let
                id =
                    Id.init t.currency t.txHash

                fn dir =
                    let
                        field =
                            case dir of
                                Incoming ->
                                    .inputs

                                Outgoing ->
                                    .outputs

                        toPair : Api.Data.TxValue -> Maybe ( Id, Api.Data.Values )
                        toPair { address, value } =
                            -- TODO what to do with multisig?
                            List.head address
                                |> Maybe.map (\a -> ( Id.init t.currency a, value ))
                    in
                    field t
                        |> Maybe.map (List.filterMap toPair)
                        |> Maybe.andThen NList.fromList
                        |> Maybe.map (NDict.fromNonemptyList >> Ok)
                        |> Maybe.withDefault (InternalError (TxValuesEmpty direction id) |> Err)

                inputs =
                    fn Incoming

                outputs =
                    fn Outgoing

                offset =
                    addressXOffset
                        / 2
                        |> Direction.signOffsetByDirection direction
            in
            Result.map2
                (\in_ out ->
                    { id = id
                    , type_ =
                        Utxo
                            { x = anchor.x + offset
                            , y = anchor.y
                            , inputs = in_
                            , outputs = out
                            }
                    }
                )
                inputs
                outputs
