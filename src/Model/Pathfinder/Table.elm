module Model.Pathfinder.Table exposing (PagedTable, decPage, getPage, incPage, nrPages, setLoading)

import Model.Graph.Table exposing (Table)
import RecordSetter exposing (s_loading)


type alias PagedTable d =
    { t : Table d
    , nrItems : Maybe Int
    , currentPage : Int
    , itemsPerPage : Int
    }


nrPages : PagedTable d -> Maybe Int
nrPages pt =
    pt.nrItems |> Maybe.map (\x -> toFloat x / toFloat pt.itemsPerPage) |> Maybe.map ceiling


incPage : PagedTable d -> PagedTable d
incPage pt =
    { pt | currentPage = pt.currentPage + 1 }


decPage : PagedTable d -> PagedTable d
decPage pt =
    let
        nextpage =
            pt.currentPage - 1
    in
    if nextpage >= 1 then
        { pt | currentPage = pt.currentPage - 1 }

    else
        pt


setLoading : Bool -> PagedTable d -> PagedTable d
setLoading l pt =
    let
        t =
            pt.t
    in
    { pt | t = t |> s_loading l }


getPage : PagedTable d -> List d
getPage pt =
    let
        drp =
            (pt.currentPage - 1) * pt.itemsPerPage
    in
    (List.drop drp >> List.take pt.itemsPerPage) pt.t.filtered
