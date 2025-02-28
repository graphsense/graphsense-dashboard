module Model.Pathfinder.PagedTable exposing (PagedTable, decPage, getPage, goToFirstPage, hasNextPage, incPage, isNextPageLoaded, nextPage, setLoading)

import Model.Graph.Table exposing (Table)
import RecordSetter exposing (s_loading)
import Util exposing (n)


type alias PagedTable d =
    { table : Table d
    , nrItems : Maybe Int
    , currentPage : Int
    , itemsPerPage : Int
    }


nextPage : (Maybe String -> List eff) -> PagedTable d -> ( PagedTable d, List eff )
nextPage load pt =
    if (pt.table.nextpage /= Nothing) && not (isNextPageLoaded pt) then
        ( pt
            |> incPage
            |> setLoading True
        , load pt.table.nextpage
        )

    else
        pt
            |> incPage
            |> n


incPage : PagedTable d -> PagedTable d
incPage pt =
    { pt
        | currentPage = pt.currentPage + 1
    }


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


goToFirstPage : PagedTable d -> PagedTable d
goToFirstPage pt =
    { pt | currentPage = 1 }


setLoading : Bool -> PagedTable d -> PagedTable d
setLoading l pt =
    let
        t =
            pt.table
    in
    { pt | table = t |> s_loading l }


getPageByNr : PagedTable d -> Int -> List d
getPageByNr pt nr =
    let
        drp =
            (nr - 1) * pt.itemsPerPage
    in
    (List.drop drp >> List.take pt.itemsPerPage) pt.table.filtered


getPage : PagedTable d -> List d
getPage pt =
    getPageByNr pt pt.currentPage


hasNextPage : PagedTable d -> Bool
hasNextPage pt =
    isNextPageLoaded pt || (pt.table.nextpage /= Nothing)


isNextPageLoaded : PagedTable d -> Bool
isNextPageLoaded pt =
    List.length (getPageByNr pt (pt.currentPage + 1)) > 0
