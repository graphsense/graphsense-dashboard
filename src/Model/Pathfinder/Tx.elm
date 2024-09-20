module Model.Pathfinder.Tx exposing (..)

import Animation exposing (Animation, Clock)
import Api.Data
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (pair)
import Util.Pathfinder exposing (getAddress)


type alias Tx =
    { id : Id
    , hovered : Bool
    , selected : Bool
    , type_ : TxType
    , x : Float
    , y : Animation
    , dx : Float
    , dy : Float
    , clock : Clock
    , opacity : Animation
    , isStartingPoint : Bool
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
    { inputs : Dict Id Io
    , outputs : Dict Id Io
    , raw : Api.Data.TxUtxo
    }


type alias Io =
    { values : Api.Data.Values
    , address : Maybe Address
    , aggregatesN : Int
    }


hasAddress : Id -> Tx -> Bool
hasAddress id tx =
    hasOutput id tx || hasInput id tx


isInFlow : Id -> Tx -> Bool
isInFlow =
    hasInput


isOutFlow : Id -> Tx -> Bool
isOutFlow =
    hasOutput


hasOutput : Id -> Tx -> Bool
hasOutput id tx =
    case tx.type_ of
        Account { to } ->
            to == id

        Utxo { outputs } ->
            Dict.get id outputs /= Nothing


hasInput : Id -> Tx -> Bool
hasInput id tx =
    case tx.type_ of
        Account { from } ->
            from == id

        Utxo { inputs } ->
            Dict.get id inputs /= Nothing


listAddressesForTx : Dict Id Address -> Tx -> List ( Direction, Address )
listAddressesForTx addresses tx =
    (case tx.type_ of
        Account { from, to } ->
            [ ( Incoming, from ), ( Outgoing, to ) ]

        Utxo { inputs, outputs } ->
            (Dict.keys inputs
                |> List.map (pair Incoming)
            )
                ++ (Dict.keys outputs
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
    Coords tx.x (Animation.animate tx.clock tx.y)
        |> Just


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


getAccountTx : Tx -> Maybe AccountTx
getAccountTx { type_ } =
    case type_ of
        Utxo _ ->
            Nothing

        Account t ->
            Just t


getRawTimestamp : Tx -> Int
getRawTimestamp tx =
    case tx.type_ of
        Account t ->
            t.raw.timestamp

        Utxo t ->
            t.raw.timestamp


getTxId : Api.Data.Tx -> Id
getTxId tx =
    case tx of
        Api.Data.TxTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.TxTxUtxo t ->
            Id.init t.currency t.txHash


getTxId2 : Api.Data.AddressTx -> Id
getTxId2 tx =
    case tx of
        Api.Data.AddressTxTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.AddressTxAddressTxUtxo t ->
            Id.init t.currency t.txHash
