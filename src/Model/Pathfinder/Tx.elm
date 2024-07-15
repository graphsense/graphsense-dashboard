module Model.Pathfinder.Tx exposing (..)

import Animation exposing (Animation, Clock)
import Api.Data
import Dict exposing (Dict)
import Dict.Nonempty as NDict exposing (NonemptyDict)
import List.Nonempty as NList
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (first, pair)
import Util.Pathfinder exposing (getAddress)


coinbasePseudoAddress : String
coinbasePseudoAddress =
    "coinbase"


type alias Tx =
    { id : Id
    , hovered : Bool
    , selected : Bool
    , type_ : TxType
    }


type TxType
    = Account AccountTx
    | Utxo UtxoTx


type alias AccountTx =
    { from : Id
    , to : Id
    , value : Api.Data.Values
    , raw : Api.Data.TxAccount
    }


type alias UtxoTx =
    { x : Float
    , y : Animation
    , dx : Float
    , dy : Float
    , clock : Clock
    , opacity : Animation
    , inputs : NonemptyDict Id Io
    , outputs : NonemptyDict Id Io
    , raw : Api.Data.TxUtxo
    }


type alias Io =
    { values : Api.Data.Values
    , visible : Bool
    , aggregatesN : Int
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


getAddressesForTx : Dict Id Address -> Tx -> List ( Direction, Address )
getAddressesForTx addresses tx =
    (case tx.type_ of
        Account { from, to } ->
            [ ( Incoming, from ), ( Outgoing, to ) ]

        Utxo { inputs, outputs } ->
            (NDict.toList inputs
                |> List.map first
                |> List.map (pair Incoming)
            )
                ++ (NDict.toList outputs
                        |> List.map first
                        |> List.map (pair Outgoing)
                   )
    )
        |> List.filterMap
            (\( dir, a ) ->
                getAddress addresses a
                    |> Result.toMaybe
                    |> Maybe.map (pair dir)
            )


calcCoords : NList.Nonempty Address -> Coords
calcCoords =
    NList.map addressToCoords >> Coords.avg


getCoords : Tx -> Maybe Coords
getCoords tx =
    case tx.type_ of
        Utxo { x, y, clock } ->
            Coords x (Animation.animate clock y)
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
    Animation.getTo y
        |> Coords x


getUtxoTx : Tx -> Maybe UtxoTx
getUtxoTx { type_ } =
    case type_ of
        Utxo t ->
            Just t

        Account _ ->
            Nothing


getRawTimestamp : Tx -> Int
getRawTimestamp tx =
    case tx.type_ of
        Account t ->
            t.raw.timestamp

        Utxo t ->
            t.raw.timestamp
