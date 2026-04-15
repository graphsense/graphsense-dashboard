module Components.Tooltip exposing (Config, Effect, Model, Msg, attributes, defaultConfig, init, mapConfig, perform, tooltipRow, tooltipRowCustomValue, update, val, view, withBackgroundColor, withBorderColor, withBorderWidth, withZIndex)

import Color exposing (Color)
import Config.View as View exposing (Config)
import Css
import Hovercard
import Html.Styled exposing (Attribute, Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onMouseOut, onMouseOver)
import RecordSetter as Rs
import Theme.Html.GraphComponents as GraphComponents
import Util exposing (n)
import Util.Css as Css
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..))
import View.Locale as Locale


type Model
    = Model ModelInternal


type alias ModelInternal =
    { hovercard : Hovercard.Model
    , closing : Bool
    , open : Bool
    }


type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { tag : Msg -> msg
    , zIndex : Int
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
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



-- | Apply a function to modify the Config


mapConfig : (a -> b) -> Config a -> Config b
mapConfig fn (Config cfg) =
    Config
        { tag = cfg.tag >> fn
        , zIndex = cfg.zIndex
        , borderWidth = cfg.borderWidth
        , backgroundColor = cfg.backgroundColor
        , borderColor = cfg.borderColor
        }


type Msg
    = OpenTooltip
    | CloseTooltip
    | HovercardMsg Hovercard.Msg


type Effect
    = HovercardCmd (Cmd Hovercard.Msg)


init : String -> ( Model, List Effect )
init id =
    let
        ( hovercard, cmd ) =
            Hovercard.init id
    in
    ( Model
        { hovercard = hovercard
        , closing = False
        , open = False
        }
    , [ HovercardCmd cmd ]
    )


attributes : Config msg -> List (Attribute msg)
attributes (Config { tag }) =
    [ OpenTooltip |> tag |> onMouseOver
    , CloseTooltip |> tag |> onMouseOut
    ]


update : Msg -> Model -> ( Model, List Effect )
update msg (Model model) =
    case msg of
        OpenTooltip ->
            { model | open = True }
                |> Model
                |> n

        CloseTooltip ->
            { model | closing = True }
                |> Model
                |> n

        HovercardMsg hm ->
            let
                ( hc, cmd ) =
                    Hovercard.update hm model.hovercard
            in
            ( Model { model | hovercard = hc }
            , [ HovercardCmd cmd ]
            )


view : Config msg -> Model -> Html msg -> Html msg
view (Config config) (Model model) content =
    if model.open then
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
                { tickLength = 16
                , zIndex = config.zIndex
                , borderColor = config.borderColor
                , backgroundColor = config.backgroundColor
                , borderWidth = config.borderWidth
                , viewport = Nothing
                }
                model.hovercard
                []
            |> Html.Styled.fromUnstyled

    else
        text ""


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
