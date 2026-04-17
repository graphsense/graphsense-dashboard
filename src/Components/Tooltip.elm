module Components.Tooltip exposing (Config, Effect, Model, Msg, attributes, defaultConfig, init, perform, reposition, subscriptions, tooltipRow, tooltipRowCustomValue, update, val, view, withBackgroundColor, withBorderColor, withBorderWidth, withCloseDelay, withFixed, withOpenDelay, withViewport, withZIndex)

import Basics.Extra exposing (flip)
import Color exposing (Color)
import Config.View as View exposing (Config)
import Css
import Hovercard
import Html.Styled exposing (Attribute, Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onClick, onMouseLeave, onMouseOver)
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


type Model a
    = Model (Maybe (ModelInternal a))


type alias ModelInternal a =
    { hovercard : Maybe Hovercard.Model
    , state : State
    , id : String
    , content : a
    }


type State
    = Open
    | Closing
    | Opening


type Config a msg
    = Config (ConfigInternal a msg)


type alias Viewport =
    { x : Float, y : Float, width : Float, height : Float }


type alias ConfigInternal a msg =
    { tag : Msg a -> msg
    , zIndex : Int
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
    , viewport : Maybe Viewport
    , fixed : Bool
    , openDelay : Float
    , closeDelay : Float
    }



-- | Create a default Config with sensible defaults


defaultConfig : (Msg a -> msg) -> Config a msg
defaultConfig tag =
    Config
        { tag = tag
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


withZIndex : Int -> Config a msg -> Config a msg
withZIndex zIndex (Config cfg) =
    Config { cfg | zIndex = zIndex }



-- | Set the border color of the Config


withBorderColor : Color -> Config a msg -> Config a msg
withBorderColor borderColor (Config cfg) =
    Config { cfg | borderColor = borderColor }



-- | Set the background color of the Config


withBackgroundColor : Color -> Config a msg -> Config a msg
withBackgroundColor backgroundColor (Config cfg) =
    Config { cfg | backgroundColor = backgroundColor }



-- | Set the border width of the Config


withBorderWidth : Float -> Config a msg -> Config a msg
withBorderWidth borderWidth (Config cfg) =
    Config { cfg | borderWidth = borderWidth }


withViewport : Viewport -> Config a msg -> Config a msg
withViewport vp (Config c) =
    Config { c | viewport = Just vp }


withFixed : Config a msg -> Config a msg
withFixed (Config c) =
    Config { c | fixed = True }


{-| Set the delay (in milliseconds) before the tooltip opens
-}
withOpenDelay : Float -> Config a msg -> Config a msg
withOpenDelay delay (Config c) =
    Config { c | openDelay = delay }


{-| Set the delay (in milliseconds) before the tooltip closes
-}
withCloseDelay : Float -> Config a msg -> Config a msg
withCloseDelay delay (Config c) =
    Config { c | closeDelay = delay }


type Msg a
    = OpenTooltip String a Float
    | CloseTooltip Float
    | HovercardMsg Hovercard.Msg
    | DelayPassed
    | OpenDelayPassed String
    | ClickTooltip
    | HoverTooltip


type Effect
    = HovercardCmd (Cmd Hovercard.Msg)
    | CloseEffect Float
    | OpenEffect String Float


init : Model a
init =
    Model Nothing


attributes : String -> Config a msg -> a -> List (Attribute msg)
attributes id (Config { tag, openDelay, closeDelay }) content =
    [ OpenTooltip id content openDelay |> tag |> onMouseOver
    , CloseTooltip closeDelay |> tag |> onMouseLeave
    , Html.Styled.Attributes.id id
    ]


update : Msg a -> Model a -> ( Model a, List Effect )
update msg (Model model) =
    case msg of
        OpenTooltip id content openDelay ->
            model
                |> Maybe.map
                    (\mo ->
                        case mo.state of
                            Closing ->
                                { mo
                                    | state = Opening
                                    , content = content
                                    , hovercard = 
                                        if mo.id /= id then
                                            Nothing
                                        else
                                            mo.hovercard
                                }
                                    |> Just
                                    |> Model
                                    |> flip pair [ OpenEffect id openDelay ]

                            _ ->
                                model |> Model |> n
                    )
                |> Maybe.withDefault
                    ({ state = Opening
                     , content = content
                     , id = id
                     , hovercard = Nothing
                     }
                        |> Just
                        |> Model
                        |> flip pair [ OpenEffect id openDelay ]
                    )

        ClickTooltip ->
            Nothing |> Model |> n

        HoverTooltip ->
            model
                |> Maybe.andThen
                    (\mo ->
                        case mo.state of
                            Closing ->
                                { mo
                                    | state = Open
                                }
                                    |> Just
                                    |> Model
                                    |> n
                                    |> Just

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault (model |> Model |> n)

        CloseTooltip closeDelay ->
            model
                |> Maybe.map
                    (\mo ->
                        { mo | state = Closing }
                            |> Just
                            |> Model
                            |> flip pair [ CloseEffect closeDelay ]
                    )
                |> Maybe.withDefault (model |> Model |> n)

        OpenDelayPassed id ->
            model
                |> Maybe.map
                    (\mo ->
                        case mo.state of
                            Opening ->
                                let
                                    ( hovercard, cmd ) =
                                        Hovercard.init id
                                in
                                { mo
                                    | state = Open
                                    , id = id
                                    , hovercard = Just hovercard
                                }
                                    |> Just
                                    |> Model
                                    |> flip pair [ HovercardCmd cmd ]

                            _ ->
                                n (Model model)
                    )
                |> Maybe.withDefault (model |> Model |> n)

        HovercardMsg hm ->
            model
                |> Maybe.andThen
                    (\mo ->
                        mo.hovercard
                            |> Maybe.map
                                (\hovercard ->
                                    let
                                        ( hc, cmd ) =
                                            Hovercard.update hm hovercard
                                    in
                                    ( Model (Just { mo | hovercard = Just hc })
                                    , [ HovercardCmd cmd ]
                                    )
                                )
                    )
                |> Maybe.withDefault (n (Model model))

        DelayPassed ->
            model
                |> Maybe.andThen
                    (\mo ->
                        if mo.state == Closing then
                            Nothing

                        else
                            model
                    )
                |> Model
                |> n


view : Config a msg -> Model a -> (a -> Html msg) -> Html msg
view (Config config) (Model model) view_ =
    model
        |> Maybe.andThen
            (\mo ->
                mo.hovercard
                    |> Maybe.map
                        (\hovercard ->
                            view_ mo.content
                                |> List.singleton
                                |> div
                                    [ css
                                        (GraphComponents.tooltipDown_details.styles
                                            ++ [ Css.minWidth (Css.px 230) ]
                                        )
                                    , ClickTooltip |> config.tag |> onClick
                                    , HoverTooltip |> config.tag |> onMouseOver
                                    , CloseTooltip config.closeDelay |> config.tag |> onMouseLeave
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
                        )
            )
        |> Maybe.withDefault none


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


perform : Effect -> Cmd (Msg a)
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


reposition : Model a -> List Effect
reposition (Model model) =
    Maybe.andThen .hovercard model
        |> Maybe.map (Hovercard.getElement >> HovercardCmd >> List.singleton)
        |> Maybe.withDefault []


subscriptions : Model a -> Sub (Msg a)
subscriptions (Model model) =
    model
        |> Maybe.andThen .hovercard
        |> Maybe.map Hovercard.subscriptions
        |> Maybe.map (Sub.map HovercardMsg)
        |> Maybe.withDefault Sub.none
