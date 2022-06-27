module Util.InfiniteScroll exposing
    ( Direction(..)
    , init, timeout, offset, direction
    , update
    , infiniteScroll, stopLoading, startLoading, isLoading
    , Model, Msg
    )

{-| Infinite scroll allows you to load more content for the user as they scroll (up or down).

The infinite scroll must be bound to an `Html` element and will execute your own `Cmd` when the user
scrolled to the bottom (or top) of the element.

The `Cmd` can be anything you want from local data fetching or complex requests on remote APIs.
All it has to do is to return a `Cmd msg` and call `stopLoading` once fetching is finished so that
the infinite scroll can continue asking for more content.


# Definitions

@docs LoadMoreCmd, Direction


# Initialization

@docs init, timeout, offset, direction, loadMoreCmd


# Update

@docs update


# Scroll

@docs infiniteScroll, stopLoading, startLoading, isLoading


# Advanced

@docs cmdFromScrollEvent, onScrollUpdate


# Types

@docs Model, Msg

-}

import Html
import Html.Attributes exposing (..)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as JD
import Process
import Task
import Time exposing (Posix)


{-| Scroll direction.

  - `Top` means new content will be asked when the user scrolls to the top of the element
  - `Bottom` means new content will be asked when the user scrolls to the bottom of the element

-}
type Direction
    = Top
    | Bottom


{-| Model of the infinite scroll module. You need to create a new one using `init` function.
-}
type Model
    = Model ModelInternal


type alias ModelInternal =
    { direction : Direction
    , offset : Int
    , isLoading : Bool
    , timeout : Float
    , lastRequest : Posix
    }


type alias ScrollPos =
    { scrollTop : Float
    , contentHeight : Int
    , containerHeight : Int
    }


{-| Infinite scroll messages you have to give to the `update` function.
-}
type Msg
    = Scroll ScrollPos
    | CurrTime Posix
    | Timeout Posix ()



-- Init


{-| Creates a new `Model`. This function needs a `LoadMoreCmd` that will be called when new data is required.

    type Msg
        = OnLoadMore InfiniteScroll.Direction

    type alias Model =
        { infiniteScroll : InfiniteScroll.Model Msg }

    loadMore : InfiniteScroll.Direction -> Cmd Msg
    loadMore dir =
        Task.perform OnLoadMore <| Task.succeed dir

    initModel : Model
    initModel =
        { infiniteScroll = InfiniteScroll.init loadMore }

-}
init : Model
init =
    Model
        { direction = Bottom
        , offset = 50
        , isLoading = False
        , timeout = 5 * 1000
        , lastRequest = Time.millisToPosix 0
        }


{-| Sets a different timeout value (default is 5 seconds)

When timeout is exceeded `stopLoading` will be automatically called so that infinite scroll can continue asking more content
event when previous request did not finished.

    init loadMore
        |> timeout (10 * 1000)

-}
timeout : Float -> Model -> Model
timeout newTimeout (Model model) =
    Model { model | timeout = newTimeout }


{-| Sets a different offset (default 50).

Offset is the number of pixels from top or bottom (depending on `Direction` value) from which infinite scroll
will detect it needs more content.

For instance with offset set to 50 and direction to `Top`. Once scroll position is 50 pixels or less from the top of the element it will require new content.
The same applies with a direction set to `Bottom` except it will check for the distance with the bottom of the element.

    init loadMore
        |> offset 100

-}
offset : Int -> Model -> Model
offset newOffset (Model model) =
    Model { model | offset = newOffset }


{-| Sets a different direction (default to `Bottom`).

A direction set to `Bottom` will check distance of the scroll bar from the bottom of the element, whereas a direction set to `Top`
will check distance of the scroll bar from the top of the element.

    init loadMore
        |> direction Top

-}
direction : Direction -> Model -> Model
direction newDirection (Model model) =
    Model { model | direction = newDirection }



-- Update


{-| The update function must be called in your own update function. It will return an updated `Model` and commands to execute.

    type Msg
        = InfiniteScrollMsg InfiniteScroll.Msg

    type alias Model =
        { infiniteScroll : InfiniteScroll.Model Msg }

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            InfiniteScrollMsg msg_ ->
                let
                    ( infiniteScroll, cmd ) =
                        InfiniteScroll.update InfiniteScrollMsg msg_ model.infiniteScroll
                in
                ( { model | infiniteScroll = infiniteScroll }, cmd )

-}
update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg (Model model) =
    case msg of
        Scroll pos ->
            scrollUpdate pos (Model model)

        CurrTime time ->
            ( Model { model | lastRequest = time }
            , Task.perform (Timeout time) <| Process.sleep model.timeout
            , False
            )

        Timeout time _ ->
            if time == model.lastRequest then
                ( stopLoading (Model model), Cmd.none, False )

            else
                ( Model model, Cmd.none, False )


shouldLoadMore : ModelInternal -> ScrollPos -> Bool
shouldLoadMore model { scrollTop, contentHeight, containerHeight } =
    if model.isLoading then
        False

    else
        case model.direction of
            Top ->
                scrollTop <= toFloat model.offset

            Bottom ->
                let
                    excessHeight =
                        contentHeight - containerHeight
                in
                scrollTop >= toFloat (excessHeight - model.offset)


scrollUpdate : ScrollPos -> Model -> ( Model, Cmd Msg, Bool )
scrollUpdate pos (Model model) =
    if shouldLoadMore model pos then
        ( startLoading (Model model)
        , Task.perform CurrTime <| Time.now
        , True
        )

    else
        ( Model model, Cmd.none, False )



-- Infinite scroll


{-| Function used to bind the infinite scroll on an element.

**The element's height must be explicitly set, otherwise scroll event won't be triggered**

    type Msg
        = InfiniteScrollMsg InfiniteScroll.Msg

    view : Model -> Html Msg
    view _ =
        let
            styles =
                [ ( "height", "300px" ) ]
        in
            div [ infiniteScroll InfiniteScrollMsg, Attributes.style styles ]
                [ -- Here will be my long list -- ]

-}
infiniteScroll : Html.Attribute Msg
infiniteScroll =
    stopPropagationOn "scroll" (JD.map (\pos -> ( Scroll pos, True )) decodeScrollPos)


{-| Starts loading more data. You should never have to use this function has it is automatically called
when new content is required and your `loadMore` command is executed.
-}
startLoading : Model -> Model
startLoading (Model model) =
    Model { model | isLoading = True }


{-| Checks if the infinite scroll is currently in a loading state.

Which means it won't ask for more data even if the user scrolls

-}
isLoading : Model -> Bool
isLoading (Model model) =
    model.isLoading


{-| Stops loading. You should call this function when you have finished fetching new data. This tells infinite scroll that it
can continue asking you more content.

If you forget to call this function or if your data fetching is too long, you will be asked to retrieve more content after timeout has expired.

-}
stopLoading : Model -> Model
stopLoading (Model model) =
    Model { model | isLoading = False }



-- Decoder


decodeScrollPos : JD.Decoder ScrollPos
decodeScrollPos =
    JD.map3 ScrollPos
        (JD.oneOf [ JD.at [ "target", "scrollTop" ] JD.float, JD.at [ "target", "scrollingElement", "scrollTop" ] JD.float ])
        (JD.oneOf [ JD.at [ "target", "scrollHeight" ] JD.int, JD.at [ "target", "scrollingElement", "scrollHeight" ] JD.int ])
        (JD.map2 Basics.max offsetHeight clientHeight)


offsetHeight : JD.Decoder Int
offsetHeight =
    JD.oneOf [ JD.at [ "target", "offsetHeight" ] JD.int, JD.at [ "target", "scrollingElement", "offsetHeight" ] JD.int ]


clientHeight : JD.Decoder Int
clientHeight =
    JD.oneOf [ JD.at [ "target", "clientHeight" ] JD.int, JD.at [ "target", "scrollingElement", "clientHeight" ] JD.int ]
