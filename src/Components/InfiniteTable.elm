module Components.InfiniteTable exposing
    ( Config
    , Fetch
    , Model
    , Msg
    , TableConfig
    , abort
    , appendData
    , config
    , getCurrentData
    , getPage
    , getPageSize
    , getTable
    , gotoFirstPage
    , infiniteScroll
    , init
    , isEmpty
    , isLoading
    , isScrolling
    , loadFirstPage
    , removeItem
    , reset
    , resetCurrent
    , setCountable
    , setData
    , setForce
    , setTriggerOffset
    , sortBy
    , update
    , updateItem
    , updateTable
    , view
    , withAbort
    , withFetch
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
import RecordSetter exposing (s_asc, s_caption, s_data, s_desc, s_filtered, s_loading, s_rowAttrs, s_state, s_table, s_tfoot)
import Result.Extra
import Table as T
import Task
import Tuple exposing (mapFirst, pair)
import Tuple3 exposing (mapThird)
import Util exposing (and)


type Model nextPage d
    = Model (ModelInternal nextPage d)


{-| Fetch data with pagination and sorting
@param sortState A tuple of (columnName, isDescending) to determine sorting
@param pageSize The number of items to fetch per page
@param nextPageHandle handle for fetching the next page of data. If Nothing, no more pages are available
@return An effectful operation that may return a string
-}
type alias Fetch nextPage eff =
    Maybe ( String, Bool ) -> Int -> Maybe nextPage -> eff


type alias ModelInternal nextPage d =
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
            { asc : ( IntDict d, Maybe nextPage, Bool )
            , desc : ( IntDict d, Maybe nextPage, Bool )
            }
    , bounce : Bounce
    , direction : Direction
    , tracker : Maybe String
    , countable : Bool
    }


type Config nextPage eff
    = Config
        { fetch : Maybe (Fetch nextPage eff)
        , force : Bool
        , effectToTracker : Maybe (eff -> Maybe String)
        , abort : Maybe (String -> eff)
        , triggerOffset : Float
        }


{-| Create a default Config for the InfiniteTable.
-}
config : Config nextPage eff
config =
    Config
        { fetch = Nothing
        , force = False
        , effectToTracker = Nothing
        , abort = Nothing
        , triggerOffset = 100
        }


{-| Set the fetch function in a Config.
-}
withFetch : Fetch nextPage eff -> Config nextPage eff -> Config nextPage eff
withFetch fetch (Config cfg) =
    Config { cfg | fetch = Just fetch }


{-| Set the force flag in a Config.
-}
setForce : Bool -> Config nextPage eff -> Config nextPage eff
setForce force (Config cfg) =
    Config { cfg | force = force }


{-| Set the abort and effectToTracker functions in a Config.

@param abortFn Function to abort a running fetch
@param effectToTracker Function to convert an effect to a tracker string

-}
withAbort : (String -> eff) -> (eff -> Maybe String) -> Config nextPage eff -> Config nextPage eff
withAbort abortFn effectToTracker (Config cfg) =
    Config
        { cfg
            | abort = Just abortFn
            , effectToTracker = Just effectToTracker
        }


{-| Set the triggerOffset in a Config.
-}
setTriggerOffset : Float -> Config nextPage eff -> Config nextPage eff
setTriggerOffset triggerOffset (Config cfg) =
    Config { cfg | triggerOffset = triggerOffset }


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


init : String -> Int -> Model nextPage d
init tableId pagesize =
    Model
        { table = Table.initUnsorted
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
        , countable = False
        }


setCountable : Bool -> Model nextPage d -> Model nextPage d
setCountable bool (Model m) =
    Model { m | countable = bool }


setData : Config nextPage eff -> Table.Filter d -> Maybe nextPage -> List d -> Model nextPage d -> ( Model nextPage d, Cmd Msg, List eff )
setData cfg filt nextpage data model =
    let
        ( m2, eff ) =
            resetCurrent cfg model
    in
    appendData cfg filt nextpage data m2
        |> mapThird ((++) eff)


appendData : Config nextPage eff -> Table.Filter d -> Maybe nextPage -> List d -> Model nextPage d -> ( Model nextPage d, Cmd Msg, List eff )
appendData cfg filt nextpage data (Model model) =
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

        nextpageNew =
            if model.countable && List.length data < model.pagesize then
                Nothing

            else
                nextpage
    in
    { model
        | table =
            model.table
                |> s_loading False
        , iterations = model.iterations + 1
    }
        |> setIntDict nextpageNew True newDict
        |> loadMore cfg
        |> getRowHeight


{-| Resets both sorting's tables
-}
reset : Config nextPage eff -> Model nextPage d -> ( Model nextPage d, List eff )
reset cfg (Model model) =
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
        |> abort cfg


{-| Resets only the current's sorting table
-}
resetCurrent : Config nextPage eff -> Model nextPage d -> ( Model nextPage d, List eff )
resetCurrent cfg (Model model) =
    { model
        | iterations = 1
        , scrollTop = 0
        , table = model.table |> s_loading False
    }
        |> setIntDict Nothing False IntDict.empty
        |> Model
        |> abort cfg


initData : { asc : ( IntDict d, Maybe nextPage, Bool ), desc : ( IntDict d, Maybe nextPage, Bool ) }
initData =
    { asc = ( IntDict.empty, Nothing, False )
    , desc = ( IntDict.empty, Nothing, False )
    }


setIntDict : Maybe nextPage -> Bool -> IntDict d -> ModelInternal nextPage d -> ModelInternal nextPage d
setIntDict nextpage loaded dict model =
    let
        ( col, isReversed ) =
            T.getSortState model.table.state

        set =
            if isReversed then
                s_asc

            else
                s_desc
    in
    Maybe.withDefault initData
        >> set ( dict, nextpage, loaded )
        >> Just
        |> flip (Dict.update col) model.data
        |> flip s_data model


getRowHeight : ( Model nextPage d, List eff ) -> ( Model nextPage d, Cmd Msg, List eff )
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


loadMore : Config nextPage eff -> ModelInternal nextPage d -> ( Model nextPage d, List eff )
loadMore (Config cfg) model =
    let
        ( len, np, loaded ) =
            getIntDict model
                |> Tuple3.mapFirst IntDict.size
    in
    if not cfg.force && loaded && np == Nothing then
        ( Model model, [] )

    else
        let
            computedContentHeight =
                model.rowHeight * toFloat len

            needsMore =
                shouldLoadMore (Config cfg)
                    model
                    { scrollTop = model.scrollTop
                    , containerHeight = model.containerHeight
                    , contentHeight = computedContentHeight
                    }
        in
        if not cfg.force && not needsMore then
            ( Model model, [] )

        else
            case cfg.fetch of
                Just fetchFn ->
                    let
                        eff =
                            fetchFn (Just (T.getSortState model.table.state)) model.pagesize np
                    in
                    ( Model
                        { model
                            | table = s_loading True model.table
                            , tracker = Maybe.withDefault (\_ -> Nothing) cfg.effectToTracker eff
                        }
                    , [ eff ]
                    )

                _ ->
                    ( Model model, [] )


getTable : Model nextPage d -> Table d
getTable (Model model) =
    model.table


removeItem : (d -> Bool) -> Model nextPage d -> Model nextPage d
removeItem predicate (Model model) =
    let
        filterDict =
            Tuple3.mapFirst (IntDict.filter (\_ value -> not (predicate value)))

        updatedData =
            Dict.map
                (\_ colData ->
                    { asc = filterDict colData.asc
                    , desc = filterDict colData.desc
                    }
                )
                model.data
    in
    { model
        | table = Table.filterTable (predicate >> not) model.table
        , data = updatedData
    }
        |> Model


updateItem : (d -> Bool) -> (d -> d) -> Model nextPage d -> Model nextPage d
updateItem predicate updateFunction (Model model) =
    let
        upd value =
            if predicate value then
                updateFunction value

            else
                value

        mapDict =
            Tuple3.mapFirst
                (IntDict.map
                    (\_ value ->
                        upd value
                    )
                )

        updatedData =
            Dict.map
                (\_ colData ->
                    { asc = mapDict colData.asc
                    , desc = mapDict colData.desc
                    }
                )
                model.data
    in
    { model
        | table =
            model.table.filtered
                |> List.map upd
                |> flip s_filtered model.table
        , data = updatedData
    }
        |> Model


update : Config nextPage eff -> Msg -> Model nextPage d -> ( Model nextPage d, Cmd Msg, List eff )
update cfg msg (Model model) =
    case msg of
        Scroll pos ->
            Model { model | bounce = Bounce.push model.bounce }
                -- don't debounce at all for now
                |> update cfg (Debounce pos)

        Debounce pos ->
            let
                bounce =
                    Bounce.pop model.bounce

                newModel =
                    { model | bounce = bounce }

                ( nnewModel, eff ) =
                    if Bounce.steady bounce then
                        scrollUpdate cfg pos newModel

                    else
                        ( Model newModel, [] )
            in
            ( nnewModel, Cmd.none, eff )

        TableMsg tm ->
            if tm == model.table.state then
                n model

            else
                Model model
                    |> abort cfg
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
                                    |> gotoFirstPage cfg
                        in
                        ( newModel, Cmd.none, eff )
                    )

        ContainerLoaded ->
            let
                ( m, eff ) =
                    gotoFirstPage cfg (Model model)
            in
            ( m, Cmd.none, eff )


n : ModelInternal nextPage d -> ( Model nextPage d, Cmd Msg, List eff )
n model =
    ( Model model, Cmd.none, [] )


updateTable : (Table d -> Table d) -> Model nextPage d -> Model nextPage d
updateTable upd (Model model) =
    { model | table = upd model.table }
        |> Model


getPageSize : Model nextPage d -> Int
getPageSize (Model model) =
    model.pagesize


getPage : Model nextPage d -> List d
getPage (Model model) =
    let
        start =
            getStart model

        end =
            start + getNumVisibleItems model
    in
    getRange start end model


getNumVisibleItems : ModelInternal nextPage d -> Int
getNumVisibleItems model =
    round model.containerHeight
        // round model.rowHeight
        + 1


loadFirstPage : Config nextPage eff -> Model nextPage d -> ( Model nextPage d, List eff )
loadFirstPage cfg =
    reset cfg
        >> and (gotoFirstPage cfg)


gotoFirstPage : Config nextPage eff -> Model nextPage d -> ( Model nextPage d, List eff )
gotoFirstPage cfg (Model model) =
    scrollUpdate cfg
        { scrollTop = 0
        , containerHeight = model.containerHeight
        , contentHeight = model.contentHeight
        }
        model


{-| Determines whether more data should be fetched based on scroll position.

This uses a dual-check strategy to handle variable-height rows correctly:


## Problem

The virtual scroller estimates total content height as `rowHeight * itemCount`,
using a single sampled row height. When rows have variable heights (e.g. the
tags table where labels/categories wrap to multiple lines), the estimate can
diverge from reality:

  - If the sampled row is TALLER than average: the estimated content height
    exceeds the actual DOM scrollHeight. The user reaches the real scroll
    bottom, but `nearComputedBottom` thinks there's still space — no fetch
    triggers, and the user gets stuck.

  - If the sampled row is SHORTER than average: estimated height is too small,
    causing premature fetching (harmless, just slightly eager).


## Solution

Two independent checks, either of which triggers a fetch:

1.  `nearComputedBottom` — classical check against the estimated content height
    (`rowHeight * itemCount`). Works well when the estimate is accurate or
    underestimates.

2.  `nearActualBottom` — fallback that compares `scrollTop + containerHeight`
    against the real DOM `scrollHeight` (passed in from scroll events via
    `model.contentHeight`). This catches the stuck-at-bottom case: even if the
    computed estimate says there's more room, if the user has physically
    scrolled to within `triggerOffset` of the actual DOM bottom, we fetch.

The `nearActualBottom` guard (`model.contentHeight > containerHeight`) prevents
false triggers when the content is shorter than the container (nothing to
scroll, so `scrollTop` is 0 and the condition would trivially be true).

-}
shouldLoadMore : Config nextPage eff -> ModelInternal nextPage d -> ScrollPos -> Bool
shouldLoadMore (Config cfg) model { scrollTop, contentHeight, containerHeight } =
    if model.table.loading then
        False

    else
        let
            excessHeight =
                contentHeight - containerHeight

            nearComputedBottom =
                scrollTop >= (excessHeight - cfg.triggerOffset)

            nearActualBottom =
                model.contentHeight
                    > containerHeight
                    && scrollTop
                    + containerHeight
                    >= model.contentHeight
                    - cfg.triggerOffset
        in
        nearComputedBottom || nearActualBottom


scrollUpdate : Config nextPage eff -> ScrollPos -> ModelInternal nextPage d -> ( Model nextPage d, List eff )
scrollUpdate (Config cfg) pos model =
    let
        newModel =
            { model
                | scrollTop = pos.scrollTop
                , contentHeight = pos.contentHeight
                , containerHeight = pos.containerHeight
                , hackyFlag = not model.hackyFlag
            }
    in
    loadMore (Config cfg) newModel


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


sortBy : String -> Bool -> Model nextPage d -> Model nextPage d
sortBy col asc (Model model) =
    Table.sortBy col asc model.table
        |> flip s_table model
        |> Model


getStart : ModelInternal nextPage data -> Int
getStart model =
    round model.scrollTop
        // round model.rowHeight


getRange : Int -> Int -> ModelInternal nextPage data -> List data
getRange start end =
    getIntDict
        >> Tuple3.first
        >> IntDict.range start end
        >> IntDict.values


view :
    TableConfig data msg
    -> List (Attribute msg)
    -> Model nextPage data
    -> Html msg
view tableConfig attributes (Model model) =
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
                            tableConfig.loadingPlaceholderAbove

                        Bottom ->
                            tableConfig.loadingPlaceholderBelow
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
                { toId = tableConfig.toId
                , toMsg = TableMsg >> tableConfig.tag
                , columns = tableConfig.columns
                , customizations =
                    tableConfig.customizations
                        |> s_caption (placeholder Top prefix)
                        |> s_tfoot (placeholder Bottom suffix)
                        |> s_rowAttrs
                            (\d ->
                                tableConfig.customizations.rowAttrs d
                                    ++ [ model.tableId ++ "_row" |> id ]
                            )
                }

        data =
            getRange start end model
                -- ATTENTION: sort here is needed to fix sorting bug of Table.applySorter
                -- which re-sort already sorted data within a group of items with same sort predicate
                |> T.getSortedData c model.table.state
    in
    div
        (css
            [ Css.overflowY Css.auto
            ]
            :: id model.tableId
            :: attributes
            ++ [ infiniteScroll tableConfig.tag ]
        )
        [ iframe
            -- here to trigger a load event when the table is rendered the first time
            [ Json.Decode.succeed (tableConfig.tag ContainerLoaded)
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


isLoading : Model nextPage data -> Bool
isLoading =
    getTable
        >> .loading


{-| True for messages emitted while the user is scrolling the table.
Useful for closing transient UI (e.g. tooltips) whose trigger row may be
detached by virtual scrolling before its mouseleave can fire.
-}
isScrolling : Msg -> Bool
isScrolling msg =
    case msg of
        Scroll _ ->
            True

        Debounce _ ->
            True

        _ ->
            False


isEmpty : Model nextPage data -> Bool
isEmpty (Model model) =
    getIntDict model
        |> Tuple3.first
        |> IntDict.isEmpty


getCurrentData : Model nextPage data -> List data
getCurrentData (Model model) =
    getIntDict model
        |> Tuple3.first
        |> IntDict.values


getIntDict : ModelInternal nextPage data -> ( IntDict data, Maybe nextPage, Bool )
getIntDict model =
    let
        ( col, isReversed ) =
            T.getSortState model.table.state
    in
    Dict.get col model.data
        |> Maybe.map
            (if isReversed then
                .asc

             else
                .desc
            )
        |> Maybe.withDefault ( IntDict.empty, Nothing, False )


abort : Config nextPage eff -> Model nextPage data -> ( Model nextPage data, List eff )
abort (Config cfg) (Model model) =
    ( Model
        { model
            | tracker = Nothing
            , table = model.table |> s_loading False
        }
    , case ( cfg.abort, model.tracker ) of
        ( Just abortFn, Just tracker ) ->
            [ abortFn tracker ]

        _ ->
            []
    )
