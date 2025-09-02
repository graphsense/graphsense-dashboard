module Components.InfiniteTable exposing
    ( Config
    , Fetch
    , Model
    , Msg
    , TableConfig
    , appendData
    , getPage
    , getPageSize
    , getTable
    , infiniteScroll
    , init
    , isEmpty
    , isLoading
    , loadFirstPage
    , removeItem
    , setData
    , sortBy
    , update
    , updateTable
    , viewTable
    )

import Basics.Extra exposing (flip, uncurry)
import Bounce exposing (Bounce)
import Browser.Dom as Dom
import Components.Table as Table exposing (Table)
import Css
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css, id, property)
import Html.Styled.Events exposing (stopPropagationOn)
import IntDict exposing (IntDict)
import Json.Decode
import Json.Encode
import RecordSetter exposing (s_caption, s_loading, s_nextpage, s_rowAttrs, s_state, s_table, s_tfoot)
import Result.Extra
import Table as T
import Task
import Tuple exposing (mapFirst, pair)


type Model d
    = Model (ModelInternal d)


type alias Fetch eff =
    Int -> Maybe String -> eff


type alias ModelInternal d =
    { tableId : String
    , table : Table d
    , pagesize : Int
    , iterations : Int
    , scrollTop : Float
    , contentHeight : Float
    , rowHeight : Float
    , containerHeight : Float
    , hackyFlag : Bool
    , data : IntDict d
    , bounce : Bounce
    , direction : Direction
    }


type alias Config eff =
    { fetch : Fetch eff
    , triggerOffset : Float
    }


type alias TableConfig data msg =
    { toId : data -> String
    , columns : List (T.Column data msg)
    , customizations : T.Customizations data msg
    , tag : Msg -> msg
    , loadingPlaceholderAbove : List (Html msg)
    , loadingPlaceholderBelow : List (Html msg)
    }


type alias ScrollPos =
    { scrollTop : Float
    , contentHeight : Float
    , containerHeight : Float
    }


type Msg
    = Scroll ScrollPos
    | Debounce ScrollPos
    | TableMsg T.State
    | GotTableElement (Result Dom.Error Dom.Element)
    | GotRowElement (Result Dom.Error Dom.Element)


type Direction
    = Top
    | Bottom


init : String -> Int -> Table d -> Model d
init tableId pagesize table =
    Model
        { table = table
        , tableId = tableId
        , pagesize = pagesize
        , iterations = 0
        , scrollTop = 0
        , contentHeight = 300
        , containerHeight = 300
        , rowHeight = 30
        , data = IntDict.empty
        , bounce = Bounce.init
        , hackyFlag = False
        , direction = Bottom
        }


appendData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Cmd Msg, Maybe eff )
appendData config filt nextpage data (Model it) =
    { it
        | table =
            Table.appendData filt data it.table
                |> s_nextpage nextpage
                |> s_loading False
        , data =
            let
                offset =
                    IntDict.size it.data
            in
            data
                |> List.indexedMap pair
                |> List.map (mapFirst ((+) offset))
                |> List.foldl (uncurry IntDict.insert) it.data
    }
        |> loadMore config False
        |> getRowHeight


setData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Cmd Msg, Maybe eff )
setData config filt nextpage data (Model it) =
    { it
        | table =
            Table.setData filt data it.table
                |> s_nextpage nextpage
                |> s_loading False
        , data =
            data
                |> List.indexedMap pair
                |> IntDict.fromList
        , iterations = 1
    }
        |> loadMore config False
        |> getRowHeight


getRowHeight : ( Model d, Maybe eff ) -> ( Model d, Cmd Msg, Maybe eff )
getRowHeight ( Model model, maybeEff ) =
    ( Model model
    , [ Dom.getElement model.tableId
            |> Task.attempt GotTableElement
      , Dom.getElement (model.tableId ++ "_row")
            |> Task.attempt GotRowElement
      ]
        |> Cmd.batch
    , maybeEff
    )


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


update : Config eff -> Msg -> Model d -> ( Model d, Cmd Msg, Maybe eff )
update config msg (Model model) =
    case msg of
        Scroll pos ->
            ( Model { model | bounce = Bounce.push model.bounce }
            , Bounce.delay 25 (Debounce pos)
            , Nothing
            )

        Debounce pos ->
            let
                bounce =
                    Bounce.pop model.bounce

                newModel =
                    { model | bounce = bounce }

                ( nnewModel, eff ) =
                    if Bounce.steady bounce then
                        scrollUpdate config
                            pos
                            { newModel
                                | scrollTop = pos.scrollTop
                                , contentHeight = pos.contentHeight
                                , containerHeight = pos.containerHeight
                                , hackyFlag = not model.hackyFlag
                            }

                    else
                        ( Model newModel, Nothing )
            in
            ( nnewModel, Cmd.none, eff )

        TableMsg tm ->
            let
                newModel =
                    s_state tm model.table
                        |> flip s_table model
                        |> Model
            in
            ( newModel, Cmd.none, Nothing )

        GotTableElement result ->
            ( Result.Extra.unwrap (Model model)
                (\el ->
                    Model { model | containerHeight = el.element.height }
                )
                result
            , Cmd.none
            , Nothing
            )

        GotRowElement result ->
            ( Result.Extra.unwrap (Model model)
                (\el ->
                    Model { model | rowHeight = el.element.height }
                )
                result
            , Cmd.none
            , Nothing
            )


updateTable : (Table d -> Table d) -> Model d -> Model d
updateTable upd (Model pt) =
    { pt | table = upd pt.table }
        |> Model


getPageSize : Model d -> Int
getPageSize (Model pt) =
    pt.pagesize


getPage : Model d -> List d
getPage (Model model) =
    let
        start =
            getStart model

        end =
            start + getNumVisibleItems model
    in
    getRange start end model


getNumVisibleItems : ModelInternal d -> Int
getNumVisibleItems model =
    round model.containerHeight
        // round model.rowHeight
        + 1


loadFirstPage : Config eff -> Model d -> ( Model d, Maybe eff )
loadFirstPage config (Model pt) =
    ( Model pt |> setLoading True
    , config.fetch pt.pagesize Nothing
        |> Just
    )


shouldLoadMore : Config eff -> ModelInternal d -> ScrollPos -> Bool
shouldLoadMore config model { scrollTop, contentHeight, containerHeight } =
    if model.table.loading then
        False

    else
        let
            excessHeight =
                contentHeight - containerHeight
        in
        scrollTop >= (excessHeight - config.triggerOffset)


scrollUpdate : Config eff -> ScrollPos -> ModelInternal d -> ( Model d, Maybe eff )
scrollUpdate config pos model =
    if shouldLoadMore config model pos then
        loadMore config True model

    else
        ( Model model, Nothing )


decodeScrollPos : Json.Decode.Decoder ScrollPos
decodeScrollPos =
    Json.Decode.map3 ScrollPos
        (Json.Decode.oneOf
            [ Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float
            , Json.Decode.at [ "target", "scrollingElement", "scrollTop" ] Json.Decode.float
            ]
        )
        (Json.Decode.oneOf
            [ Json.Decode.at [ "target", "scrollHeight" ] Json.Decode.float
            , Json.Decode.at [ "target", "scrollingElement", "scrollHeight" ] Json.Decode.float
            ]
        )
        (Json.Decode.map2 Basics.max offsetHeight clientHeight)


offsetHeight : Json.Decode.Decoder Float
offsetHeight =
    Json.Decode.oneOf
        [ Json.Decode.at [ "target", "offsetHeight" ] Json.Decode.float
        , Json.Decode.at [ "target", "scrollingElement", "offsetHeight" ] Json.Decode.float
        ]


clientHeight : Json.Decode.Decoder Float
clientHeight =
    Json.Decode.oneOf
        [ Json.Decode.at [ "target", "clientHeight" ] Json.Decode.float
        , Json.Decode.at [ "target", "scrollingElement", "clientHeight" ] Json.Decode.float
        ]


infiniteScroll : (Msg -> msg) -> Attribute msg
infiniteScroll mapper =
    Html.Styled.Attributes.map mapper <|
        stopPropagationOn "scroll" (Json.Decode.map (\pos -> ( Scroll pos, True )) decodeScrollPos)


sortBy : String -> Bool -> Model d -> Model d
sortBy col desc (Model model) =
    Table.sortBy col desc model.table
        |> flip s_table model
        |> Model


getStart : ModelInternal data -> Int
getStart model =
    round model.scrollTop
        // round model.rowHeight


getRange : Int -> Int -> ModelInternal data -> List data
getRange start end =
    .data
        >> IntDict.range start end
        >> IntDict.values


viewTable :
    TableConfig data msg
    -> List (Attribute msg)
    -> Model data
    -> Html msg
viewTable config attributes (Model model) =
    let
        visibleArea =
            round model.containerHeight
                // round model.rowHeight
                // 2
                * 2
                |> max 2

        start =
            getStart model
                // visibleArea
                * visibleArea
                - visibleArea
                |> max 0

        prefix =
            toFloat start
                * model.rowHeight

        end =
            start
                + visibleArea
                * 3
                |> min (IntDict.size model.data)

        suffix =
            (IntDict.size model.data - end |> toFloat)
                * model.rowHeight

        placeholder dir height =
            let
                ldngHtml =
                    case dir of
                        Top ->
                            config.loadingPlaceholderAbove

                        Bottom ->
                            config.loadingPlaceholderBelow
            in
            if model.table.loading && model.direction == dir then
                ldngHtml
                    |> T.HtmlDetails []
                    |> Just

            else
                height
                    |> String.fromFloat
                    |> (++) "width: 100%; height: "
                    |> flip (++) "px"
                    |> Json.Encode.string
                    |> property "style"
                    |> List.singleton
                    |> flip div
                        (if height > 0 then
                            ldngHtml

                         else
                            []
                        )
                    |> List.singleton
                    |> T.HtmlDetails []
                    |> Just

        c =
            T.customConfig
                { toId = config.toId
                , toMsg = TableMsg >> config.tag
                , columns = config.columns
                , customizations =
                    config.customizations
                        |> s_caption (placeholder Top prefix)
                        |> s_tfoot (placeholder Bottom suffix)
                        |> s_rowAttrs
                            (\d ->
                                config.customizations.rowAttrs d
                                    ++ [ model.tableId ++ "_row" |> id ]
                            )
                }

        data =
            getRange start end model
    in
    div
        (css
            [ Css.overflowY Css.auto
            ]
            :: id model.tableId
            :: attributes
            ++ [ infiniteScroll config.tag ]
        )
        [ -- this is needed to force rerendering of the whole table
          -- otherwise a scroll event would be triggered by dom changes
          if model.hackyFlag then
            T.view c model.table.state data

          else
            T.view c model.table.state data
                |> List.singleton
                |> div []
        ]


isLoading : Model data -> Bool
isLoading =
    getTable
        >> .loading


isEmpty : Model data -> Bool
isEmpty (Model model) =
    IntDict.isEmpty model.data
