module Init.Pathfinder.RelationDetails exposing (getExposedAssetsForNeighbor, getExposedAssetsForNeighborWebData, init)

import Api.Data
import Config.Update as Update
import Dict
import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Maybe.Extra
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.RelationDetails as RelationDetails
import RemoteData
import Time
import Util.Data as Data


getExposedAssetsForNeighbor : Api.Data.NeighborAddress -> Maybe (List String)
getExposedAssetsForNeighbor data =
    if Data.isAccountLike data.address.currency then
        String.toUpper data.address.currency
            :: (data.tokenValues
                    |> Maybe.map (Dict.keys >> List.map String.toUpper)
                    |> Maybe.withDefault []
               )
            |> Just

    else
        Nothing


getExposedAssetsForNeighborWebData : RemoteData.WebData (Maybe Api.Data.NeighborAddress) -> Maybe (List String)
getExposedAssetsForNeighborWebData webData =
    webData
        |> RemoteData.toMaybe
        |> Maybe.Extra.join
        |> Maybe.andThen getExposedAssetsForNeighbor


init : Update.Config -> AggEdge -> ( Time.Posix, Time.Posix ) -> RelationDetails.Model
init uc edge ( rangeFrom, rangeTo ) =
    let
        -- if data should be missing, fall back to empty asset list
        -- update later assigns data when available
        a2bAssets =
            edge.a2b |> getExposedAssetsForNeighborWebData

        b2aAssets =
            edge.b2a |> getExposedAssetsForNeighborWebData
    in
    { a2bTableOpen = False
    , b2aTableOpen = False
    , a2bTable = RelationTxsTable.init uc ( rangeFrom, rangeTo ) a2bAssets
    , b2aTable = RelationTxsTable.init uc ( rangeFrom, rangeTo ) b2aAssets
    , aggEdge = edge
    , rangeFrom = rangeFrom
    , rangeTo = rangeTo
    }
