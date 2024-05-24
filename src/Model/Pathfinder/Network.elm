module Model.Pathfinder.Network exposing (..)

import Dict exposing (Dict)
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx exposing (Tx)


type alias Network =
    { addresses : Dict Id Address
    , txs : Dict Id Tx
    }


hasTx : Id -> Network -> Bool
hasTx id network =
    Dict.member id network.txs


hasAddress : Id -> Network -> Bool
hasAddress id network =
    Dict.member id network.addresses


listTxsForAddress : Network -> Id -> List ( Direction, Tx )
listTxsForAddress network id =
    network.txs
        |> Dict.values
        |> List.filterMap
            (\tx ->
                if Tx.hasInput id tx then
                    Just ( Incoming, tx )

                else if Tx.hasOutput id tx then
                    Just ( Outgoing, tx )

                else
                    Nothing
            )


selectAddress : Network -> Id -> Network
selectAddress =
    setSelectedAddress True


unSelectAddress : Network -> Id -> Network
unSelectAddress =
    setSelectedAddress False


unSelectAll : Network -> Network
unSelectAll network =
    { network | addresses = Dict.map (\_ x -> { x | selected = False }) network.addresses }


setSelectedAddress : Bool -> Network -> Id -> Network
setSelectedAddress bool network id =
    let
        entry =
            Dict.get id network.addresses |> Maybe.map (\x -> { x | selected = bool })
    in
    case entry of
        Just x ->
            { network | addresses = Dict.insert id x network.addresses }

        Nothing ->
            network
