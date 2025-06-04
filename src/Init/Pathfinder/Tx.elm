module Init.Pathfinder.Tx exposing (fromTxAccountData, fromTxUtxoData)

import Animation as A
import Api.Data
import Dict
import Dict.Extra
import Init.Pathfinder.Id as Id
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Tx exposing (Io, Tx, TxType(..))
import Util.Data


fromTxAccountData : Api.Data.TxAccount -> Coords -> Tx
fromTxAccountData tx coords =
    let
        id =
            Id.init tx.network tx.identifier
    in
    { id = id
    , hovered = False
    , selected = False
    , isStartingPoint = False
    , x = coords.x
    , y = A.static coords.y
    , dx = 0
    , dy = 0
    , opacity = A.static 1
    , clock = 0
    , type_ =
        let
            from =
                Id.init tx.network tx.fromAddress

            to =
                Id.init tx.network tx.toAddress
        in
        Account
            { from = from
            , to = to
            , fromAddress = Nothing
            , toAddress = Nothing
            , value = tx.value
            , raw = tx
            }
    }


fromTxUtxoData : Api.Data.TxUtxo -> Coords -> Tx
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

        groupedIos =
            Dict.Extra.groupBy .address (getAddressValuesIntermediates tx.outputs True ++ getAddressValuesIntermediates tx.inputs False)

        sumIoEntries addr l =
            let
                value =
                    List.foldl Util.Data.addValues Util.Data.valuesZero (List.map .values l)
            in
            { address = addr, cnt = List.length l, value = value, isOutput = value.value > 0 }

        summedIo =
            Dict.map sumIoEntries groupedIos

        fn dir =
            Dict.values summedIo
                |> List.filter (\x -> x.isOutput == (dir == Outgoing))
                |> List.map
                    (\ioEntry ->
                        let
                            id_ =
                                Id.init tx.currency ioEntry.address
                        in
                        ( id_
                        , initIo ioEntry.value ioEntry.cnt
                        )
                    )
    in
    { id = id
    , hovered = False
    , selected = False
    , isStartingPoint = False
    , x = coords.x
    , y = A.static coords.y
    , dx = 0
    , dy = 0
    , opacity = A.static 1
    , clock = 0
    , type_ =
        let
            inputs =
                fn Incoming
                    |> Dict.fromList
        in
        Utxo
            { inputs = inputs
            , outputs =
                fn Outgoing
                    |> List.filter
                        (\( o, _ ) -> Dict.member o inputs |> not)
                    |> Dict.fromList
            , raw = tx
            }
    }


initIo : Api.Data.Values -> Int -> Io
initIo values aggregatesN =
    { values = Util.Data.absValues values
    , address = Nothing
    , aggregatesN = aggregatesN
    }
