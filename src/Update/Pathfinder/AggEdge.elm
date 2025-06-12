module Update.Pathfinder.AggEdge exposing (setAddress, setLoading, setRelationData, updateAddress)

import Api.Data
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..), WebData)


updateAddress : Id -> (Address -> Address) -> AggEdge -> AggEdge
updateAddress id upd aggEdge =
    if id == aggEdge.a then
        { aggEdge
            | aAddress = Maybe.map upd aggEdge.aAddress
        }

    else if id == aggEdge.b then
        { aggEdge
            | bAddress = Maybe.map upd aggEdge.bAddress
        }

    else
        aggEdge


setAddress : Maybe Address -> AggEdge -> AggEdge
setAddress ma edge =
    ma
        |> Maybe.map
            (\a ->
                if a.id == edge.a then
                    { edge | aAddress = Just a }

                else if a.id == edge.b then
                    { edge | bAddress = Just a }

                else
                    edge
            )
        |> Maybe.withDefault edge


setRelationData : Id -> Direction -> WebData Api.Data.NeighborAddress -> AggEdge -> AggEdge
setRelationData id direction data edge =
    case ( id == edge.a, id == edge.b, direction ) of
        ( True, False, Outgoing ) ->
            { edge | a2b = data }

        ( False, True, Outgoing ) ->
            { edge | b2a = data }

        ( True, False, Incoming ) ->
            { edge | b2a = data }

        ( False, True, Incoming ) ->
            { edge | a2b = data }

        _ ->
            edge


setLoading : Direction -> Id -> AggEdge -> AggEdge
setLoading dir id =
    setRelationData id dir Loading
