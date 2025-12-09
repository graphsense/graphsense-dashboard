module Components.InfiniteTable exposing
    ( Config
    , Fetch
    , Model
    , Msg
    , TableConfig
    , abort
    , appendData
    , getPage
    , getPageSize
    , getTable
    , gotoFirstPage
    , infiniteScroll
    , init
    , isEmpty
    , isLoading
    , loadFirstPage
    , removeItem
    , reset
    , resetCurrent
    , setData
    , sortBy
    , update
    , updateTable
    , view
    )

import Basics.Extra exposing (flip, uncurry)
import Bounce exposing (Bounce)
import Browser.Dom as Dom
import Components.Table as Table exposing (Table)
import Css
import Dict exposing (Dict)
import Html.Styled exposing (Attribute, Html, div, iframe)
import Html.Styled.Attributes exposing (css, height, id, property)
import Html.Styled.Events exposing (on, stopPropagationOn)
import IntDict exposing (IntDict)
import Json.Decode
import Json.Encode
import Maybe.Extra
import RecordSetter exposing (s_asc, s_caption, s_data, s_desc, s_loading, s_rowAttrs, s_state, s_table, s_tfoot)
import Result.Extra
import Table as T
import Task
import Tuple exposing (mapFirst, pair)
import Tuple3 exposing (mapThird)
import Util exposing (and)


type Model d
    = Model (ModelInternal d)


type alias Fetch eff =
    Maybe ( String, Bool ) -> Int -> Maybe String -> eff


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

    -- mapping column name to two intdicts, one for asc the other for desc order
    , data :
        Dict
            String
            { asc : ( IntDict d, Maybe String, Bool )
            , desc : ( IntDict d, Maybe String, Bool )
            }
    , bounce : Bounce
    , direction : Direction
    , tracker : Maybe String
    }


type alias Config eff =
    { fetch : Fetch eff
    , force : Bool
    , effectToTracker : eff -> Maybe String
    , abort : String -> eff
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
    | NoOp
    | ScrolledToTop T.State (Result Dom.Error ())
    | ContainerLoaded


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
        , data = Dict.empty
        , bounce = Bounce.init
        , hackyFlag = False
        , direction = Bottom
        , tracker = Nothing
        }


setData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Cmd Msg, List eff )
setData config filt nextpage data model =
    let
        ( m2, eff ) =
            resetCurrent config model
    in
    appendData config filt nextpage data m2
        |> mapThird ((++) eff)


appendData : Config eff -> Table.Filter d -> Maybe String -> List d -> Model d -> ( Model d, Cmd Msg, List eff )
appendData config filt nextpage data (Model model) =
    let
        dict =
            getIntDict model
                |> Tuple3.first

        offset =
            IntDict.size dict

        newDict =
            data
                |> List.filter filt.filter
                |> List.indexedMap pair
                |> List.map (mapFirst ((+) offset))
                |> List.foldl (uncurry IntDict.insert) dict
    in
    { model
        | table =
            model.table
                |> s_loading False
        , iterations = model.iterations + 1
    }
        |> setIntDict nextpage True newDict
        |> loadMore config
        |> getRowHeight


{-| Resets both sorting's tables
-}
reset : Config eff -> Model d -> ( Model d, List eff )
reset config (Model model) =
    let
        ( col, _ ) =
            T.getSortState model.table.state
    in
    Model
        { model
            | iterations = 1
            , scrollTop = 0
            , data = Dict.insert col initData model.data
        }
        |> abort config


{-| Resets only the current's sorting table
-}
resetCurrent : Config eff -> Model d -> ( Model d, List eff )
resetCurrent config (Model model) =
    { model
        | iterations = 1
        , scrollTop = 0
        , table = model.table |> s_loading False
    }
        |> setIntDict Nothing False IntDict.empty
        |> Model
        |> abort config


initData : { asc : ( IntDict d, Maybe String, Bool ), desc : ( IntDict d, Maybe String, Bool ) }
initData =
    { asc = ( IntDict.empty, Nothing, False )
    , desc = ( IntDict.empty, Nothing, False )
    }


setIntDict : Maybe String -> Bool -> IntDict d -> ModelInternal d -> ModelInternal d
setIntDict nextpage loaded dict model =
    let
        ( col, isReversed ) =
            T.getSortState model.table.state

        set =
            if isReversed then
                s_desc

            else
                s_asc
    in
    Maybe.withDefault initData
        >> set ( dict, nextpage, loaded )
        >> Just
        |> flip (Dict.update col) model.data
        |> flip s_data model


getRowHeight : ( Model d, List eff ) -> ( Model d, Cmd Msg, List eff )
getRowHeight ( Model model, listEff ) =
    ( Model model
    , [ Dom.getElement model.tableId
            |> Task.attempt GotTableElement
      , Dom.getElement (model.tableId ++ "_row")
            |> Task.attempt GotRowElement
      ]
        |> Cmd.batch
    , listEff
    )


loadMore : Config eff -> ModelInternal d -> ( Model d, List eff )
loadMore config model =
    let
        ( len, np, loaded ) =
            getIntDict model
                |> Tuple3.mapFirst IntDict.size
    in
    if not config.force && loaded && np == Nothing then
        ( Model model, [] )

    else
        let
            needsMore =
                shouldLoadMore config
                    model
                    { scrollTop = model.scrollTop
                    , containerHeight = model.containerHeight
                    , contentHeight = model.rowHeight * toFloat len
                    }
        in
        if not config.force && not needsMore then
            ( Model model, [] )

        else
            let
                eff =
                    config.fetch (Just (T.getSortState model.table.state)) model.pagesize np
            in
            ( Model
                { model
                    | table = s_loading True model.table
                    , tracker = config.effectToTracker eff
                }
            , [ eff ]
            )


getTable : Model d -> Table d
getTable (Model model) =
    model.table


removeItem : (d -> Bool) -> Model d -> Model d
removeItem predicate (Model model) =
    { model
        | table = Table.filterTable (predicate >> not) model.table
    }
        |> Model


update : Config eff -> Msg -> Model d -> ( Model d, Cmd Msg, List eff )
update config msg (Model model) =
    case msg of
        Scroll pos ->
            Model { model | bounce = Bounce.push model.bounce }
                -- don't debounce at all for now
                |> update config (Debounce pos)

        Debounce pos ->
            let
                bounce =
                    Bounce.pop model.bounce

                newModel =
                    { model | bounce = bounce }

                ( nnewModel, eff ) =
                    if Bounce.steady bounce then
                        scrollUpdate config pos newModel

                    else
                        ( Model newModel, [] )
            in
            ( nnewModel, Cmd.none, eff )

        TableMsg tm ->
            if tm == model.table.state then
                n model

            else
                Model model
                    |> abort config
                    |> (\( m, eff ) ->
                            ( m
                            , Dom.setViewportOf model.tableId 0 0
                                |> Task.attempt (ScrolledToTop tm)
                            , eff
                            )
                       )

        GotTableElement result ->
            ( Result.Extra.unwrap (Model model)
                (\el ->
                    Model { model | containerHeight = el.element.height }
                )
                result
            , Cmd.none
            , []
            )

        GotRowElement result ->
            ( Result.Extra.unwrap (Model model)
                (\el ->
                    Model { model | rowHeight = el.element.height }
                )
                result
            , Cmd.none
            , []
            )

        NoOp ->
            n model

        ScrolledToTop tm result ->
            result
                |> Result.Extra.unwrap (n model)
                    (\_ ->
                        let
                            ( newModel, eff ) =
                                s_state tm model.table
                                    |> flip s_table model
                                    |> Model
                                    |> gotoFirstPage config
                        in
                        ( newModel, Cmd.none, eff )
                    )

        ContainerLoaded ->
            let
                ( m, eff ) =
                    gotoFirstPage config (Model model)
            in
            ( m, Cmd.none, eff )


n : ModelInternal d -> ( Model d, Cmd Msg, List eff )
n model =
    ( Model model, Cmd.none, [] )


updateTable : (Table d -> Table d) -> Model d -> Model d
updateTable upd (Model model) =
    { model | table = upd model.table }
        |> Model


getPageSize : Model d -> Int
getPageSize (Model model) =
    model.pagesize


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


loadFirstPage : Config eff -> Model d -> ( Model d, List eff )
loadFirstPage config =
    reset config
        >> and (gotoFirstPage config)


gotoFirstPage : Config eff -> Model d -> ( Model d, List eff )
gotoFirstPage config (Model model) =
    scrollUpdate config
        { scrollTop = 0
        , containerHeight = model.containerHeight
        , contentHeight = model.contentHeight
        }
        model


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


scrollUpdate : Config eff -> ScrollPos -> ModelInternal d -> ( Model d, List eff )
scrollUpdate config pos model =
    let
        newModel =
            { model
                | scrollTop = pos.scrollTop
                , contentHeight = pos.contentHeight
                , containerHeight = pos.containerHeight
                , hackyFlag = not model.hackyFlag
            }
    in
    loadMore config newModel


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
    getIntDict
        >> Tuple3.first
        >> IntDict.range start end
        >> IntDict.values


view :
    TableConfig data msg
    -> List (Attribute msg)
    -> Model data
    -> Html msg
view config attributes (Model model) =
    let
        dict =
            getIntDict model
                |> Tuple3.first

        visibleItems =
            round model.containerHeight
                // round model.rowHeight
                // 2
                * 2
                + 2

        start =
            getStart model
                // visibleItems
                * visibleItems
                - visibleItems
                |> max 0

        prefix =
            toFloat start
                * model.rowHeight

        end =
            start
                + visibleItems
                * 3
                |> min (IntDict.size dict)

        suffix =
            (IntDict.size dict - end |> toFloat)
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
                        (if height > 0 && model.table.loading then
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
        [ iframe
            -- here to trigger a load event when the table is rendered the first time
            [ Json.Decode.succeed (config.tag ContainerLoaded)
                |> on "load"
            , height 0
            ]
            []
        , -- this is needed to force rerendering of the whole table
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
    getIntDict model
        |> Tuple3.first
        |> IntDict.isEmpty


getIntDict : ModelInternal data -> ( IntDict data, Maybe String, Bool )
getIntDict model =
    let
        ( col, isReversed ) =
            T.getSortState model.table.state
    in
    Dict.get col model.data
        |> Maybe.map
            (if isReversed then
                .desc

             else
                .asc
            )
        |> Maybe.withDefault ( IntDict.empty, Nothing, False )


abort : Config eff -> Model data -> ( Model data, List eff )
abort conf (Model model) =
    ( Model
        { model
            | tracker = Nothing
            , table = model.table |> s_loading False
        }
    , Maybe.map conf.abort model.tracker
        |> Maybe.Extra.toList
    )
