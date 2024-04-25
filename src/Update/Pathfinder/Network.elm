module Update.Pathfinder.Network exposing (addAddress, addTx, updateAddress)

import Config.Pathfinder exposing (addressXOffset, addressYOffset)
import Dict
import Dict.Nonempty as NDict
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Id.Address as Address
import Model.Pathfinder.Id.Tx as Tx
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx exposing (getAddressesForTx)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (s_data, s_incomingTxs, s_outgoingTxs)
import RemoteData exposing (RemoteData(..))
import Result.Extra
import Set
import Tuple exposing (first, pair, second)


addAddress : Id -> Network -> Network
addAddress id model =
    case Dict.get id model.addresses of
        Nothing ->
            case findAddressPosition id model of
                NextToTx tx ->
                    case tx.type_ of
                        Tx.Utxo t ->
                            placeAddressNextToUtxoTx t id model

                        Tx.Account t ->
                            placeAddressNextToAccountTx t id model

                InBetween coords ->
                    placeAddressInBetween coords id model

                Outside coords ->
                    placeAddress coords id model

        Just _ ->
            model


placeAddressNextToAccountTx : Tx.AccontTx -> Id -> Network -> Network
placeAddressNextToAccountTx arg1 arg2 arg3 =
    Debug.todo "TODO"


placeAddressNextToUtxoTx : Tx.UtxoTx -> Id -> Network -> Network
placeAddressNextToUtxoTx tx id model =
    let
        toSiblings io =
            io
                |> NDict.toList
                |> List.filterMap (first >> (\a -> Dict.get a model.addresses))

        ( direction, siblings ) =
            if NDict.get id tx.outputs /= Nothing then
                ( Outgoing, toSiblings tx.outputs )

            else
                ( Incoming, toSiblings tx.inputs )

        coords =
            { x = tx.x + Direction.signOffsetByDirection direction addressXOffset
            , y =
                getMaxAddressY siblings
                    |> Maybe.withDefault tx.y
            }
    in
    placeAddressInBetween coords id model


placeAddressInBetween : Coords -> Id -> Network -> Network
placeAddressInBetween coords id model =
    let
        ( above, below ) =
            model.addresses
                |> Dict.values
                |> List.partition
                    (\a -> a.y <= coords.y)

        diff y =
            y - coords.y

        add d addr =
            { addr | y = addr.y + d }

        moveAddresses subset addresses =
            Maybe.map diff
                >> Maybe.map
                    (\d -> List.map (add d) subset)
                >> Maybe.map
                    (List.foldl (\a -> Dict.insert a.id a) addresses)
                >> Maybe.withDefault addresses

        newAddresses =
            getMaxAddressY above
                |> moveAddresses above model.addresses
                |> (\addresses ->
                        getMinAddressY below
                            |> moveAddresses below addresses
                   )
    in
    { model | addresses = newAddresses }
        |> placeAddress coords id


placeAddress : Coords -> Id -> Network -> Network
placeAddress coords id model =
    { model | addresses = Dict.insert id (Address.init id coords) model.addresses }


updateAddress : Id -> (Address -> Address) -> Network -> Network
updateAddress id update model =
    { model | addresses = Dict.update id (Maybe.map update) model.addresses }


loadAddress : Plugins -> Coords -> Id -> Network -> ( Network, List Effect )
loadAddress _ coords id model =
    let
        na =
            coords
                |> Address.init id
                |> s_data Loading
    in
    ( { model
        | addresses = Dict.insert id na model.addresses
      }
    , BrowserGotAddressData id
        |> Api.GetAddressEffect
            { currency = Id.network id
            , address = Id.id id
            }
        |> ApiEffect
        |> List.singleton
    )


findFreePosition : Network -> Coords
findFreePosition model =
    { x = 0
    , y =
        model.addresses
            |> Dict.values
            |> getMinAddressY
            |> Maybe.map ((+) addressYOffset)
            |> Maybe.withDefault 0
    }


getMaxAddressY : List Address -> Maybe Float
getMaxAddressY =
    List.map .y
        >> List.maximum


getMinAddressY : List Address -> Maybe Float
getMinAddressY =
    List.map .y
        >> List.minimum


type Position
    = NextToTx Tx.Tx
    | InBetween Coords
    | Outside Coords


findAddressPosition : Id -> Network -> Position
findAddressPosition id network =
    listTxsForAddress network id
        |> NList.fromList
        |> Maybe.andThen
            (\list ->
                if NList.length list == 1 then
                    NList.head list
                        |> NextToTx
                        |> Just

                else
                    list
                        |> NList.foldl
                            (\tx lst ->
                                Tx.getCoords tx
                                    |> Maybe.map (\t -> t :: lst)
                                    |> Maybe.withDefault lst
                            )
                            []
                        |> NList.fromList
                        |> Maybe.map (Coords.avg >> InBetween)
            )
        |> Maybe.withDefault (findFreePosition network |> Outside)


addTx : Tx.Tx -> Network -> Network
addTx tx network =
    let
        upd addr =
            let
                ( get, set ) =
                    if Tx.hasOutput addr.id tx then
                        ( .incomingTxs, s_incomingTxs )

                    else
                        ( .outgoingTxs, s_outgoingTxs )
            in
            if Set.member tx.id <| get addr then
                addr

            else
                set (Set.insert tx.id <| get addr) addr
    in
    getAddressesForTx network.addresses tx
        |> List.foldl
            (\a nw ->
                { nw
                    | addresses =
                        Dict.update a.id
                            (Maybe.map
                                upd
                            )
                            nw.addresses
                }
            )
            { network
                | txs = Dict.insert tx.id tx network.txs
            }
