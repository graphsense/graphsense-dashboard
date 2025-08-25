module Components.InfiniteTable exposing (Config, Fetch, Model, Msg, appendData, getPageSize, getTable, infiniteScroll, init, loadFirstPage, onScrollUpdate, removeItem, setData, sortBy, update, updateTable)

import Basics.Extra exposing (flip)
import Components.Table as Table exposing (Table)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes
import Html.Styled.Events exposing (stopPropagationOn)
import Json.Decode
import RecordSetter exposing (s_loading, s_nextpage, s_table)


type Model d
    = Model (ModelInternal d)


type alias Fetch eff =
    Int -> Maybe String -> eff


type alias ModelInternal d =
    { table : Table d
    , pagesize : Int
    , iterations : Int
    }


type alias Config eff =
    { fetch : Fetch eff
    }


type Msg
    = Scroll ScrollPos


offset : Int
offset =
    50


init : Int -> Table d -> Model d
init pagesize table =
    Model
        { table = table
        , pagesize = pagesize
        , iterations = 0
        }


appendData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Maybe eff )
appendData config filt nextpage data (Model it) =
    { it
        | table =
            Table.appendData filt data it.table
                |> s_nextpage nextpage
                |> s_loading False
    }
        |> loadMore config False


setData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Maybe eff )
setData config filt nextpage data (Model it) =
    { it
        | table =
            Table.setData filt data it.table
                |> s_nextpage nextpage
                |> s_loading False
        , iterations = 1
    }
        |> loadMore config False


loadMore : Config eff -> Bool -> ModelInternal d -> ( Model d, Maybe eff )
loadMore config force pt =
    let
        len =
            List.length pt.table.filtered

        needsMore =
            pt.iterations * pt.pagesize > len
    in
    if len > 0 && pt.table.nextpage == Nothing then
        ( Model pt, Nothing )

    else if not force && not needsMore then
        ( Model pt, Nothing )

    else
        ( Model
            { pt
                | table = s_loading True pt.table
                , iterations = pt.iterations + 1
            }
        , config.fetch pt.pagesize pt.table.nextpage
            |> Just
        )


setLoading : Bool -> Model d -> Model d
setLoading l (Model pt) =
    { pt | table = pt.table |> s_loading l } |> Model


getTable : Model d -> Table d
getTable (Model pt) =
    pt.table


removeItem : (d -> Bool) -> Model d -> Model d
removeItem predicate (Model pt) =
    { pt
        | table = Table.filterTable (predicate >> not) pt.table
    }
        |> Model


update : Config eff -> Msg -> Model d -> ( Model d, Maybe eff )
update config msg (Model pt) =
    case msg of
        Scroll pos ->
            scrollUpdate config pos pt


updateTable : (Table d -> Table d) -> Model d -> Model d
updateTable upd (Model pt) =
    { pt | table = upd pt.table }
        |> Model


getPageSize : Model d -> Int
getPageSize (Model pt) =
    pt.pagesize


loadFirstPage : Config eff -> Model d -> ( Model d, Maybe eff )
loadFirstPage config (Model pt) =
    ( Model pt |> setLoading True
    , config.fetch pt.pagesize Nothing
        |> Just
    )


shouldLoadMore : ModelInternal d -> ScrollPos -> Bool
shouldLoadMore model { scrollTop, contentHeight, containerHeight } =
    if model.table.loading then
        False

    else
        let
            excessHeight =
                contentHeight - containerHeight
        in
        scrollTop >= toFloat (excessHeight - offset)


scrollUpdate : Config eff -> ScrollPos -> ModelInternal d -> ( Model d, Maybe eff )
scrollUpdate config pos model =
    if shouldLoadMore model pos then
        loadMore config True model

    else
        ( Model model, Nothing )


type alias ScrollPos =
    { scrollTop : Float
    , contentHeight : Int
    , containerHeight : Int
    }


decodeScrollPos : Json.Decode.Decoder ScrollPos
decodeScrollPos =
    Json.Decode.map3 ScrollPos
        (Json.Decode.oneOf [ Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float, Json.Decode.at [ "target", "scrollingElement", "scrollTop" ] Json.Decode.float ])
        (Json.Decode.oneOf [ Json.Decode.at [ "target", "scrollHeight" ] Json.Decode.int, Json.Decode.at [ "target", "scrollingElement", "scrollHeight" ] Json.Decode.int ])
        (Json.Decode.map2 Basics.max offsetHeight clientHeight)


offsetHeight : Json.Decode.Decoder Int
offsetHeight =
    Json.Decode.oneOf [ Json.Decode.at [ "target", "offsetHeight" ] Json.Decode.int, Json.Decode.at [ "target", "scrollingElement", "offsetHeight" ] Json.Decode.int ]


clientHeight : Json.Decode.Decoder Int
clientHeight =
    Json.Decode.oneOf [ Json.Decode.at [ "target", "clientHeight" ] Json.Decode.int, Json.Decode.at [ "target", "scrollingElement", "clientHeight" ] Json.Decode.int ]


onScrollUpdate : Config eff -> Json.Decode.Value -> Model d -> ( Model d, Maybe eff )
onScrollUpdate config value (Model model) =
    case Json.Decode.decodeValue decodeScrollPos value of
        Ok pos ->
            scrollUpdate config pos model

        Err _ ->
            ( Model model, Nothing )


infiniteScroll : (Msg -> msg) -> Attribute msg
infiniteScroll mapper =
    Html.Styled.Attributes.map mapper <|
        stopPropagationOn "scroll" (Json.Decode.map (\pos -> ( Scroll pos, True )) decodeScrollPos)


sortBy : String -> Bool -> Model d -> Model d
sortBy col desc (Model model) =
    Table.sortBy col desc model.table
        |> flip s_table model
        |> Model
