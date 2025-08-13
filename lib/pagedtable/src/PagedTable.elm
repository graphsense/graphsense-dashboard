module PagedTable exposing (Config, Model, Msg(..), appendData, getCurrentPage, getItemsPerPage, getNrItems, getPage, getTable, goToFirstPage, hasNextPage, hasPrevPage, init, loadFirstPage, removeItem, setData, setItemsPerPage, setNrItems, update, updateTable, hasData)

import Init.Graph.Table as Table
import Model.Graph.Table as Table exposing (Table)
import RecordSetter exposing (s_loading, s_nextpage)
import Update.Graph.Table as Table


type Model d
    = Model (ModelInternal d)


type alias Fetch eff =
    Int -> Maybe String -> eff


type alias Config eff =
    { fetch : Maybe (Fetch eff)
    }


type alias ModelInternal d =
    { table : Table d
    , nrItems : Maybe Int
    , currentPage : Int
    , itemsPerPage : Int
    , loaded : Bool
    }


type Msg
    = NextPage
    | PrevPage
    | FirstPage


init : Table d -> Model d
init table =
    Model
        { table = table
        , nrItems = Nothing
        , currentPage = 1
        , itemsPerPage = 10
        , loaded = False
        }


setNrItems : Int -> Model d -> Model d
setNrItems nrItems (Model pt) =
    { pt | nrItems = Just nrItems }
        |> Model


getNrItems : Model d -> Maybe Int
getNrItems (Model m) =
    m.nrItems

hasData : Model d -> Bool
hasData (Model pt) =
    pt.table.filtered /= []


setItemsPerPage : Int -> Model d -> Model d
setItemsPerPage itemsPerPage (Model pt) =
    { pt | itemsPerPage = itemsPerPage }
        |> Model


n : Model d -> ( Model d, Maybe eff )
n m =
    ( m, Nothing )


update : Config eff -> Msg -> Model d -> ( Model d, Maybe eff )
update config msg (Model pt) =
    case msg of
        NextPage ->
            if hasNextPage (Model pt) then
                pt
                    |> incPage
                    |> loadMore config

            else
                pt
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


setLoading : Bool -> Model d -> Model d
setLoading l (Model pt) =
    let
        t =
            pt.table
    in
    { pt | table = t |> s_loading l } |> Model


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
    isNextPageFullyLoaded pt || (pt.table.nextpage /= Nothing)


pageItemsNeeded : ModelInternal d -> Int -> Int
pageItemsNeeded pt pageNr =
    pt.itemsPerPage - List.length (getPageByNr pt pageNr)


appendData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Maybe eff )
appendData config filt nextPage data (Model pt) =
    { pt
        | table =
            Table.appendData filt data pt.table
                |> s_nextpage nextPage
                |> s_loading False
        , loaded = True
    }
        |> loadMore config


setData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Maybe eff )
setData config filt nextPage data (Model pt) =
    { pt
        | table =
            Table.setData filt data pt.table
                |> s_nextpage nextPage
                |> s_loading False
        , loaded = True
    }
        |> loadMore config


loadMore : Config eff -> ModelInternal d -> ( Model d, Maybe eff )
loadMore config pt =
    let
        pagesize =
            pt.currentPage
                |> pageItemsNeeded pt

        fetch =
            if pagesize == 0 then
                Nothing

            else if pt.table.nextpage == Nothing && pt.loaded then
                Nothing

            else
                config.fetch
                    |> Maybe.map (\fn -> fn pagesize pt.table.nextpage)
    in
    ( pt |> Model
         |> setLoading (fetch /= Nothing)
        
    , fetch
    )


isNextPageFullyLoaded : ModelInternal d -> Bool
isNextPageFullyLoaded pt =
    isPageFullyLoaded pt <| pt.currentPage + 1


isPageFullyLoaded : ModelInternal d -> Int -> Bool
isPageFullyLoaded pt nr =
    pageItemsNeeded pt nr < pt.itemsPerPage



{-
   let
       pageLength =
           List.length (getPageByNr pt pageNr)

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
-}


getTable : Model d -> Table d
getTable (Model pt) =
    pt.table


removeItem : Config eff -> (d -> Bool) -> Model d -> ( Model d, Maybe eff )
removeItem config predicate (Model pt) =
    { pt
        | table = Table.filterTable (predicate >> not) pt.table
    }
        |> loadMore config


updateTable : Config eff -> (Table d -> Table d) -> Model d -> ( Model d, Maybe eff )
updateTable config upd (Model pt) =
    { pt | table = upd pt.table }
        |> loadMore config


getCurrentPage : Model d -> Int
getCurrentPage (Model pt) =
    pt.currentPage


getItemsPerPage : Model d -> Int
getItemsPerPage (Model pt) =
    pt.itemsPerPage


loadFirstPage : Config eff -> Model d -> ( Model d, Maybe eff )
loadFirstPage config (Model pt) =
    (( Model pt) |> setLoading True |> goToFirstPage
    , config.fetch
        |> Maybe.map
            (\fn -> fn pt.itemsPerPage Nothing)
    )
