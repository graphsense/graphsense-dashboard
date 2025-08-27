module Model.Pathfinder.Tx exposing
    ( AccountTx
    , Io
    , Tx
    , TxType(..)
    , UtxoTx
    , addressToCoords
    , avg
    , calcCoords
    , getAccountTx
    , getAccountTxRaw
    , getAsset
    , getCoords
    , getInputAddressIds
    , getInputs
    , getNetwork
    , getOutputAddressIds
    , getOutputs
    , getRawBaseTxHashForTx
    , getRawTimestamp
    , getRawTimestampForRelationTx
    , getTxId
    , getTxIdForAddressTx
    , getTxIdForRelationTx
    , getTxIdForTx
    , getUtxoTx
    , hasAddress
    , hasInput
    , hasOutput
    , ioToId
    , isRawInFlow
    , isRawOutFlow
    , listAddressesForTx
    , listSeparatedAddressesForTx
    , toFinalCoords
    )

import Animation exposing (Animation, Clock)
import Api.Data
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import List.Extra
import List.Nonempty as NList
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (Error(..), InternalError(..))
import Model.Pathfinder.Id as Id exposing (Id)
import Tuple exposing (pair)


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
    , fromAddress : Maybe Address -- the address present on the graph
    , toAddress : Maybe Address -- the address present on the graph
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
    , address : Maybe Address -- the address present on the graph
    , aggregatesN : Int
    }


getAsset : Tx -> String
getAsset tx =
    case tx.type_ of
        Account { raw } ->
            raw.currency

        Utxo { raw } ->
            raw.currency


getNetwork : Tx -> String
getNetwork tx =
    case tx.type_ of
        Account { raw } ->
            raw.network

        Utxo { raw } ->
            raw.currency


hasAddress : Id -> Tx -> Bool
hasAddress id tx =
    hasOutput id tx || hasInput id tx


isRawInFlow : Id -> Tx -> Bool
isRawInFlow id tx =
    case tx.type_ of
        Account { raw } ->
            raw.fromAddress == Id.id id

        Utxo { raw } ->
            raw.inputs
                |> findTxValueByAddress (Id.id id)
                |> (/=) Nothing


isRawOutFlow : Id -> Tx -> Bool
isRawOutFlow id tx =
    case tx.type_ of
        Account { raw } ->
            raw.toAddress == Id.id id

        Utxo { raw } ->
            raw.outputs
                |> findTxValueByAddress (Id.id id)
                |> (/=) Nothing


findTxValueByAddress : String -> Maybe (List Api.Data.TxValue) -> Maybe Api.Data.TxValue
findTxValueByAddress id =
    Maybe.andThen
        (List.Extra.find (.address >> List.member id))


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


listAddressesForTx : Tx -> List ( Direction, Address )
listAddressesForTx tx =
    case tx.type_ of
        Account { fromAddress, toAddress } ->
            [ fromAddress
                |> Maybe.map (pair Incoming)
            , toAddress
                |> Maybe.map (pair Outgoing)
            ]
                |> List.filterMap identity

        Utxo { inputs, outputs } ->
            (Dict.values inputs
                |> List.filterMap .address
                |> List.map (pair Incoming)
            )
                ++ (Dict.values outputs
                        |> List.filterMap .address
                        |> List.map (pair Outgoing)
                   )


listSeparatedAddressesForTx : Tx -> ( List Address, List Address )
listSeparatedAddressesForTx tx =
    case tx.type_ of
        Account { fromAddress, toAddress } ->
            ( fromAddress
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , toAddress
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            )

        Utxo { inputs, outputs } ->
            ( Dict.values inputs
                |> List.filterMap .address
            , Dict.values outputs
                |> List.filterMap .address
            )


getInputAddressIds : Tx -> List Id
getInputAddressIds tx =
    case tx.type_ of
        Account { from } ->
            [ from ]

        Utxo { raw } ->
            raw.inputs
                |> Maybe.withDefault []
                |> List.concatMap .address
                |> List.map (Id.init raw.currency)


getOutputAddressIds : Tx -> List Id
getOutputAddressIds tx =
    case tx.type_ of
        Account { to } ->
            [ to ]

        Utxo { raw } ->
            raw.outputs
                |> Maybe.withDefault []
                |> List.concatMap .address
                |> List.map (Id.init raw.currency)


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
addressToCoords =
    toFinalCoords


toFinalCoords : { t | x : Float, y : Animation } -> Coords
toFinalCoords { x, y } =
    Coords x (Animation.getTo y)


getUtxoTx : Tx -> Maybe UtxoTx
getUtxoTx { type_ } =
    case type_ of
        Utxo t ->
            Just t

        Account _ ->
            Nothing


getAccountTxRaw : Api.Data.Tx -> Maybe Api.Data.TxAccount
getAccountTxRaw tx =
    case tx of
        Api.Data.TxTxAccount t ->
            Just t

        _ ->
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


getTxIdForTx : Tx -> Id
getTxIdForTx tx =
    case tx.type_ of
        Account t ->
            Id.init t.raw.network t.raw.identifier

        Utxo t ->
            Id.init t.raw.currency t.raw.txHash


getRawBaseTxHashForTx : Tx -> String
getRawBaseTxHashForTx tx =
    case tx.type_ of
        Account t ->
            t.raw.txHash

        Utxo t ->
            t.raw.txHash


getTxIdForAddressTx : Api.Data.AddressTx -> Id
getTxIdForAddressTx tx =
    case tx of
        Api.Data.AddressTxTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.AddressTxAddressTxUtxo t ->
            Id.init t.currency t.txHash


getTxIdForRelationTx : Api.Data.Link -> Id
getTxIdForRelationTx tx =
    case tx of
        Api.Data.LinkTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.LinkLinkUtxo t ->
            Id.init t.currency t.txHash


getRawTimestampForRelationTx : Api.Data.Link -> Int
getRawTimestampForRelationTx tx =
    case tx of
        Api.Data.LinkTxAccount t ->
            t.timestamp

        Api.Data.LinkLinkUtxo t ->
            t.timestamp


ioToId : String -> Api.Data.TxValue -> Maybe Id
ioToId network =
    .address
        >> List.head
        >> Maybe.map (Id.init network)


getOutputs : Tx -> List Id
getOutputs tx =
    case tx.type_ of
        Utxo { outputs } ->
            Dict.keys outputs

        Account { to } ->
            [ to ]


getInputs : Tx -> List Id
getInputs tx =
    case tx.type_ of
        Utxo { inputs } ->
            Dict.keys inputs

        Account { from } ->
            [ from ]
