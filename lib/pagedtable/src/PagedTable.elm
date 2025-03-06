module PagedTable exposing (Config, Model, Msg, appendData, getPage, getTable, hasNextPage, hasPrevPage, init, removeItem, setItemsPerPage, setNrItems, update)

import Init.Graph.Table as Table
import Model.Graph.Table as Table exposing (Table)
import RecordSetter exposing (s_loading, s_nextpage)
import Update.Graph.Table as Table


type Model d
    = Model (ModelInternal d)


type alias Fetch eff =
    Int -> Maybe String -> eff


type alias Config eff =
    { fetch : Fetch eff
    }


type alias ModelInternal d =
    { table : Table d
    , nrItems : Maybe Int
    , currentPage : Int
    , itemsPerPage : Int
    }


type Msg
    = NextPage
    | PrevPage
    | FirstPage


init : Model d
init =
    Model
        { table = Table.initUnsorted
        , nrItems = Nothing
        , currentPage = 1
        , itemsPerPage = 10
        }


setNrItems : Int -> Model d -> Model d
setNrItems nrItems (Model pt) =
    { pt | nrItems = Just nrItems }
        |> Model


setItemsPerPage : Int -> Model d -> Model d
setItemsPerPage itemsPerPage (Model pt) =
    { pt | itemsPerPage = itemsPerPage }
        |> Model


n : Model d -> ( Model d, Maybe eff )
n m =
    ( m, Nothing )


update : Config eff -> Msg -> Model d -> ( Model d, Maybe eff )
update { fetch } msg (Model pt) =
    case msg of
        NextPage ->
            if (pt.table.nextpage /= Nothing) && not (isNextPageLoaded pt) then
                ( pt
                    |> incPage
                    |> setLoading True
                    |> Model
                , fetch pt.itemsPerPage pt.table.nextpage
                    |> Just
                )

            else
                pt
                    |> incPage
                    |> Model
                    |> n

        PrevPage ->
            let
                nextpage =
                    pt.currentPage - 1
            in
            (if nextpage >= 1 then
                { pt | currentPage = pt.currentPage - 1 }

             else
                pt
            )
                |> Model
                |> n

        FirstPage ->
            Model pt
                |> goToFirstPage
                |> n


incPage : ModelInternal d -> ModelInternal d
incPage pt =
    { pt
        | currentPage = pt.currentPage + 1
    }


goToFirstPage : Model d -> Model d
goToFirstPage (Model pt) =
    Model { pt | currentPage = 1 }


setLoading : Bool -> ModelInternal d -> ModelInternal d
setLoading l pt =
    let
        t =
            pt.table
    in
    { pt | table = t |> s_loading l }


getPageByNr : ModelInternal d -> Int -> List d
getPageByNr pt nr =
    let
        drp =
            (nr - 1) * pt.itemsPerPage
    in
    (List.drop drp >> List.take pt.itemsPerPage) pt.table.filtered


getPage : Model d -> List d
getPage (Model pt) =
    getPageByNr pt pt.currentPage


hasPrevPage : Model d -> Bool
hasPrevPage (Model pt) =
    pt.currentPage > 1


hasNextPage : Model d -> Bool
hasNextPage (Model pt) =
    isNextPageLoaded pt || (pt.table.nextpage /= Nothing)


isNextPageLoaded : ModelInternal d -> Bool
isNextPageLoaded pt =
    let
        nextPageLength =
            List.length (getPageByNr pt (pt.currentPage + 1))

        totalNumberOfPages =
            pt.nrItems
                |> Maybe.map
                    (\nrItems -> ceiling (toFloat nrItems / toFloat pt.itemsPerPage))
                |> Maybe.withDefault 0
    in
    nextPageLength
        >= pt.itemsPerPage
        || (totalNumberOfPages
                - 1
                == pt.currentPage
                && nextPageLength
                > 0
           )


appendData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Maybe eff )
appendData config filter nextPage data (Model pt) =
    { pt
        | table =
            Table.appendData filter data pt.table
                |> s_nextpage nextPage
                |> s_loading False
    }
        |> loadMore config


loadMore : Config eff -> ModelInternal d -> ( Model d, Maybe eff )
loadMore config pt =
    let
        filterRatio =
            toFloat (List.length pt.table.filtered)
                / toFloat (List.length pt.table.data)

        pagesize =
            filterRatio
                * toFloat pt.itemsPerPage
                |> ceiling

        fetch =
            if isNextPageLoaded pt then
                Nothing

            else
                config.fetch pagesize pt.table.nextpage
                    |> Just
    in
    ( pt
        |> setLoading (fetch /= Nothing)
        |> Model
    , fetch
    )


getTable : Model d -> Table d
getTable (Model pt) =
    pt.table


removeItem : Config eff -> (d -> Bool) -> Model d -> ( Model d, Maybe eff )
removeItem config predicate (Model pt) =
    { pt
        | table = Table.filterTable (predicate >> not) pt.table
    }
        |> loadMore config
