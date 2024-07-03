module Init.Pathfinder.Tx exposing (fromTxAccountData, fromTxUtxoData)

import Animation as A
import Api.Data
import Dict.Nonempty as NDict
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Io, Tx, TxType(..))


fromTxAccountData : Api.Data.TxAccount -> Tx
fromTxAccountData tx =
    let
        id =
            Id.init tx.currency tx.txHash
    in
    { id = id
    , hovered = False
    , selected = False
    , type_ =
        Account
            { from = Id.init tx.currency tx.fromAddress
            , to = Id.init tx.currency tx.toAddress
            , value = tx.value
            , raw = tx
            }
    }


fromTxUtxoData : Api.Data.TxUtxo -> Coords -> Maybe Tx
fromTxUtxoData tx coords =
    let
        id =
            Id.init tx.currency tx.txHash

        fn dir =
            let
                field =
                    case dir of
                        Incoming ->
                            .inputs

                        Outgoing ->
                            .outputs

                toPair : Api.Data.TxValue -> Maybe ( Id, Io )
                toPair { address, value } =
                    -- TODO what to do with multisig?
                    List.head address
                        |> Maybe.map (\a -> ( Id.init tx.currency a, Io value False ))
            in
            field tx
                |> Maybe.map (List.filterMap toPair)
                |> Maybe.andThen NList.fromList
    in
    Maybe.map2
        (\in_ out ->
            { id = id
            , hovered = False
            , selected = False
            , type_ =
                let
                    inputs =
                        NDict.fromNonemptyList in_
                in
                Utxo
                    { x = coords.x
                    , y = A.static coords.y
                    , dx = 0
                    , dy = 0
                    , opacity = A.static 1
                    , clock = 0
                    , inputs = inputs
                    , outputs =
                        out
                            |> NList.filter
                                (\( o, _ ) -> NDict.get o inputs == Nothing)
                                (NList.head out)
                            |> NDict.fromNonemptyList
                    , raw = tx
                    }
            }
        )
        (fn Incoming)
        (fn Outgoing)
