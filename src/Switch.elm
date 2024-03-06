module Switch exposing (Switch, attributes, duration, knobStyle, offStyle, onStyle, render, switch)

import Css exposing (..)
import Css.Transitions as T
import Html.Styled exposing (Attribute, Html, input)
import Html.Styled.Attributes exposing (css, type_)


type Switch msg units
    = Switch (Config msg units)


type alias Config msg units =
    { width : Float
    , height : Float
    , unit : Float -> ExplicitLength units
    , duration : Float
    , offStyle : List Style
    , onStyle : List Style
    , knobStyle : List Style
    , disabledStyle : List Style
    , attributes : List (Attribute msg)
    }


{-| Initialize the switch given width, height and a elm-css unit.

    switch 3 1 Css.rem

-}
switch : Float -> Float -> (Float -> ExplicitLength units) -> Switch msg units
switch width height unit =
    Switch
        { offStyle =
            [ backgroundColor <| rgba 255 255 255 0.2
            ]
        , knobStyle =
            [ backgroundColor <| rgb 255 255 255
            , property "box-shadow" "0 0 0.25em rgba(0,0,0,0.3)"
            ]
        , disabledStyle =
            [ backgroundColor <| rgb 120 120 120
            ]
        , onStyle = []
        , width = width
        , height = height
        , unit = unit
        , duration = 200
        , attributes = []
        }


{-| Render switch.

    switch 3 1 Css.rem |> render

-}
render : Switch msg units -> Html msg
render (Switch config) =
    input
        ([ type_ "checkbox"
         , toggleCss config |> css
         ]
            ++ config.attributes
        )
        []


duration : Float -> Switch msg unit -> Switch msg unit
duration d (Switch config) =
    { config | duration = d }
        |> Switch


offStyle : List Style -> Switch msg unit -> Switch msg unit
offStyle d (Switch config) =
    { config | offStyle = d }
        |> Switch


onStyle : List Style -> Switch msg unit -> Switch msg unit
onStyle d (Switch config) =
    { config | onStyle = d }
        |> Switch


knobStyle : List Style -> Switch msg unit -> Switch msg unit
knobStyle d (Switch config) =
    { config | knobStyle = d }
        |> Switch


attributes : List (Attribute msg) -> Switch msg unit -> Switch msg unit
attributes d (Switch config) =
    { config | attributes = d }
        |> Switch


toggleCss : Config msg units -> List Style
toggleCss config =
    config.offStyle
        ++ [ property "appearance" "none"
           , width <| config.unit config.width
           , height <| config.unit config.height
           , borderRadius <| config.unit config.height
           , position relative
           , cursor pointer
           , outline none
           , T.transition
                [ T.left3 config.duration 0 T.easeInOut
                , T.background3 config.duration 0 T.easeInOut
                ]
           , after <|
                config.knobStyle
                    ++ [ position absolute
                       , property "content" "\"\""
                       , width <| config.unit config.height
                       , height <| config.unit config.height
                       , borderRadius <| pct 50
                       , transform (scale 0.7)
                       , left <| px 0
                       , T.transition
                            [ T.left3 config.duration 0 T.easeInOut
                            , T.background3 config.duration 0 T.easeInOut
                            ]
                       , checked
                            [ left <| calc (pct 100) minus (config.unit config.height)
                            ]
                       ]
           , disabled <| [ after <| config.disabledStyle ]
           , checked config.onStyle
           ]
