module Init.Pathfinder.Tx exposing (fromTxAccountData, fromTxUtxoData)

import Animation as A
import Api.Data
import Dict
import Dict.Extra
import Dict.Nonempty as NDict
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Io, Tx, TxType(..))
import Monocle.Compose exposing (isoWithIso)
import Util.Data


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

        createIoIntermediate isOut x =
            List.head x.address
                |> Maybe.map
                    (\addr ->
                        { address = addr
                        , values =
                            if isOut then
                                x.value

                            else
                                Util.Data.negateValues x.value
                        }
                    )

        getAddressValuesIntermediates io isOut =
            io
                |> Maybe.map (List.filterMap (createIoIntermediate isOut))
                |> Maybe.withDefault []

        inputsWithCoinbase = (if (tx.coinbase) then (Just [{address=["coinbase"], value = Util.Data.valuesZero}]) else tx.inputs)

        groupedIos =
            Dict.Extra.groupBy .address ((getAddressValuesIntermediates tx.outputs True) ++ (getAddressValuesIntermediates inputsWithCoinbase False))

        sumIoEntries addr l =
            let
                value =
                    List.foldl Util.Data.addValues Util.Data.valuesZero (List.map .values l)
            in
            { address = addr, cnt = List.length l, value = value, isOutput = value.value > 0 }

        summedIo =
            Dict.map sumIoEntries groupedIos

        fn dir =
            Dict.toList summedIo
                |> List.map Tuple.second
                |> List.filter (\x -> x.isOutput == (dir == Outgoing))
                |> List.map (\ioEntry -> ( Id.init tx.currency ioEntry.address, Io (Util.Data.absValues ioEntry.value) False ioEntry.cnt ))
                |> NList.fromList
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
