module Components.Tooltip exposing (Config, Effect, Model, Msg, attributes, defaultConfig, init, perform, reposition, tooltipRow, tooltipRowCustomValue, update, val, view, withBackgroundColor, withBorderColor, withBorderWidth, withCloseDelay, withFixed, withOpenDelay, withViewport, withZIndex)

import Basics.Extra exposing (flip)
import Color exposing (Color)
import Config.View as View exposing (Config)
import Css
import Hovercard
import Html.Styled exposing (Attribute, Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onMouseLeave, onMouseOver)
import Process
import RecordSetter as Rs
import Task
import Theme.Html.GraphComponents as GraphComponents
import Tuple exposing (pair)
import Util exposing (n)
import Util.Css as Css
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..))
import Util.View exposing (none)
import View.Locale as Locale


type Model
    = Model ModelInternal


type alias ModelInternal =
    { hovercard : Maybe Hovercard.Model
    , state : State
    }


type State
    = Open String
    | Closing
    | Closed
    | Opening


type Config msg
    = Config (ConfigInternal msg)


type alias Viewport =
    { x : Float, y : Float, width : Float, height : Float }


type alias ConfigInternal msg =
    { tag : Msg -> msg
    , zIndex : Int
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
    , viewport : Maybe Viewport
    , fixed : Bool
    , openDelay : Float
    , closeDelay : Float
    , id : String
    }



-- | Create a default Config with sensible defaults


defaultConfig : String -> (Msg -> msg) -> Config msg
defaultConfig id tag =
    Config
        { id = id
        , tag = tag
        , zIndex = 0
        , borderColor = Color.black
        , backgroundColor = Color.white
        , borderWidth = 1.0
        , viewport = Nothing
        , fixed = False
        , openDelay = 0
        , closeDelay = 0
        }



-- | Set the z-index of the Config


withZIndex : Int -> Config msg -> Config msg
withZIndex zIndex (Config cfg) =
    Config { cfg | zIndex = zIndex }



-- | Set the border color of the Config


withBorderColor : Color -> Config msg -> Config msg
withBorderColor borderColor (Config cfg) =
    Config { cfg | borderColor = borderColor }



-- | Set the background color of the Config


withBackgroundColor : Color -> Config msg -> Config msg
withBackgroundColor backgroundColor (Config cfg) =
    Config { cfg | backgroundColor = backgroundColor }



-- | Set the border width of the Config


withBorderWidth : Float -> Config msg -> Config msg
withBorderWidth borderWidth (Config cfg) =
    Config { cfg | borderWidth = borderWidth }


withViewport : Viewport -> Config msg -> Config msg
withViewport vp (Config c) =
    Config { c | viewport = Just vp }


withFixed : Config msg -> Config msg
withFixed (Config c) =
    Config { c | fixed = True }


{-| Set the delay (in milliseconds) before the tooltip opens
-}
withOpenDelay : Float -> Config msg -> Config msg
withOpenDelay delay (Config c) =
    Config { c | openDelay = delay }


{-| Set the delay (in milliseconds) before the tooltip closes
-}
withCloseDelay : Float -> Config msg -> Config msg
withCloseDelay delay (Config c) =
    Config { c | closeDelay = delay }


type Msg
    = OpenTooltip String Float
    | CloseTooltip Float
    | HovercardMsg Hovercard.Msg
    | DelayPassed
    | OpenDelayPassed String


type Effect
    = HovercardCmd (Cmd Hovercard.Msg)
    | CloseEffect Float
    | OpenEffect String Float


init : Model
init =
    Model
        { hovercard = Nothing
        , state = Closed
        }


attributes : Config msg -> List (Attribute msg)
attributes (Config { id, tag, openDelay, closeDelay }) =
    [ OpenTooltip id openDelay |> tag |> onMouseOver
    , CloseTooltip closeDelay |> tag |> onMouseLeave
    , Html.Styled.Attributes.id id
    ]


update : Msg -> Model -> ( Model, List Effect )
update msg (Model model) =
    case msg of
        OpenTooltip id openDelay ->
            case model.state of
                Open _ ->
                    model |> Model |> n

                Opening ->
                    model |> Model |> n

                _ ->
                    { model
                        | state = Opening
                    }
                        |> Model
                        |> flip pair [ OpenEffect id openDelay ]

        CloseTooltip closeDelay ->
            { model | state = Closing }
                |> Model
                |> flip pair [ CloseEffect closeDelay ]

        OpenDelayPassed id ->
            case model.state of
                Opening ->
                    let
                        ( hovercard, cmd ) =
                            Hovercard.init id
                    in
                    { model
                        | state = Open id
                        , hovercard = Just hovercard
                    }
                        |> Model
                        |> flip pair [ HovercardCmd cmd ]

                _ ->
                    n (Model model)

        HovercardMsg hm ->
            model.hovercard
                |> Maybe.map
                    (\hovercard ->
                        let
                            ( hc, cmd ) =
                                Hovercard.update hm hovercard
                        in
                        ( Model { model | hovercard = Just hc }
                        , [ HovercardCmd cmd ]
                        )
                    )
                |> Maybe.withDefault (n (Model model))

        DelayPassed ->
            (if model.state == Closing then
                { model
                    | hovercard = Nothing
                    , state = Closed
                }

             else
                model
            )
                |> Model
                |> n


view : Config msg -> Model -> Html msg -> Html msg
view (Config config) (Model model) content =
    case model.hovercard of
        Just hovercard ->
            if Open config.id /= model.state then
                none

            else
                content
                    |> List.singleton
                    |> div
                        [ css
                            (GraphComponents.tooltipDown_details.styles
                                ++ [ Css.minWidth (Css.px 230) ]
                            )
                        ]
                    |> toUnstyled
                    |> List.singleton
                    |> Hovercard.view
                        (Hovercard.defaultConfig
                            |> Hovercard.withTickLength 16
                            |> Hovercard.withZIndex config.zIndex
                            |> Hovercard.withBorderColor config.borderColor
                            |> Hovercard.withBackgroundColor config.backgroundColor
                            |> Hovercard.withBorderWidth config.borderWidth
                            |> Hovercard.withViewport config.viewport
                            |> Hovercard.withFixed config.fixed
                        )
                        hovercard
                        []
                    |> Html.Styled.fromUnstyled

        _ ->
            none


val : View.Config -> String -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
val vc str =
    { firstRowText = Locale.string vc.locale str
    , secondRowText = ""
    , secondRowVisible = False
    }


baseRowStyle : List Css.Style
baseRowStyle =
    [ Css.width (Css.pct 100) ]


tooltipRow : { tooltipRowLabel : { title : String }, tooltipRowValue : { firstRowText : String, secondRowVisible : Bool, secondRowText : String } } -> Html msg
tooltipRow =
    GraphComponents.tooltipRowWithAttributes
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_root [ css baseRowStyle ]
            |> Rs.s_tooltipRowLabel [ css [ Css.minWidth (Css.px 90) ] ]
            |> Rs.s_firstValue [ css [ Css.property "white-space" "wrap", Css.textAlign Css.right ] ]
        )


tooltipRowCustomValue : String -> Html msg -> Html msg
tooltipRowCustomValue title rowValue =
    GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_root [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances |> Rs.s_tooltipRowValue (Just rowValue))
        { tooltipRowLabel = { title = title }
        , tooltipRowValue =
            { firstRowText = "", secondRowText = "", secondRowVisible = False }
        }


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        HovercardCmd cmd ->
            Cmd.map HovercardMsg cmd

        CloseEffect delay ->
            Process.sleep delay
                |> Task.map (\_ -> DelayPassed)
                |> Task.perform identity

        OpenEffect id delay ->
            Process.sleep delay
                |> Task.map (\_ -> OpenDelayPassed id)
                |> Task.perform identity


reposition : Model -> List Effect
reposition (Model { hovercard }) =
    Maybe.map (Hovercard.getElement >> HovercardCmd >> List.singleton) hovercard
        |> Maybe.withDefault []
