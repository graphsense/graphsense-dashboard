module Components.Tooltip exposing (Config, Effect, Model, Msg, attributes, defaultConfig, init, mapConfig, perform, reposition, tooltipRow, tooltipRowCustomValue, update, val, view, withBackgroundColor, withBorderColor, withBorderWidth, withDelay, withFixed, withViewport, withZIndex)

import Basics.Extra exposing (flip)
import Color exposing (Color)
import Config.View as View exposing (Config)
import Css
import Hovercard
import Html.Styled exposing (Attribute, Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css, id, title)
import Html.Styled.Events exposing (onMouseLeave, onMouseOut, onMouseOver)
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
    , id : String
    , state : State
    , delay : Float
    }


type State
    = Open
    | Closing
    | Closed


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
    }



-- | Create a default Config with sensible defaults


defaultConfig : (Msg -> msg) -> Config msg
defaultConfig tag =
    Config
        { tag = tag
        , zIndex = 0
        , borderColor = Color.black
        , backgroundColor = Color.white
        , borderWidth = 1.0
        , viewport = Nothing
        , fixed = False
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



-- | Apply a function to modify the Config


mapConfig : (a -> b) -> Config a -> Config b
mapConfig fn (Config cfg) =
    Config
        { tag = cfg.tag >> fn
        , zIndex = cfg.zIndex
        , borderWidth = cfg.borderWidth
        , backgroundColor = cfg.backgroundColor
        , borderColor = cfg.borderColor
        , viewport = cfg.viewport
        , fixed = cfg.fixed
        }


type Msg
    = OpenTooltip
    | CloseTooltip
    | HovercardMsg Hovercard.Msg
    | DelayPassed


type Effect
    = HovercardCmd (Cmd Hovercard.Msg)
    | CloseEffect Float


init : String -> Model
init id =
    Model
        { hovercard = Nothing
        , id = id
        , state = Closed
        , delay = 0
        }


withDelay : Float -> Model -> Model
withDelay delay (Model model) =
    Model { model | delay = delay }


attributes : Config msg -> Model -> List (Attribute msg)
attributes (Config { tag }) (Model model) =
    [ OpenTooltip |> tag |> onMouseOver
    , CloseTooltip |> tag |> onMouseLeave
    , id model.id
    ]


update : Msg -> Model -> ( Model, List Effect )
update msg (Model model) =
    case msg of
        OpenTooltip ->
            if model.state /= Open then
                let
                    ( hovercard, cmd ) =
                        Hovercard.init model.id
                in
                { model
                    | state = Open
                    , hovercard = Just hovercard
                }
                    |> Model
                    |> flip pair [ HovercardCmd cmd ]

            else
                n (Model model)

        CloseTooltip ->
            { model
                | state = Closing
            }
                |> Model
                |> flip pair [ CloseEffect model.delay ]

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


reposition : Model -> List Effect
reposition (Model { hovercard }) =
    Maybe.map (Hovercard.getElement >> HovercardCmd >> List.singleton) hovercard
        |> Maybe.withDefault []
