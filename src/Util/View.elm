module Util.View exposing (..)

import Browser.Dom as Dom
import Color
import Config.View as View
import Css exposing (Color, Style)
import Css.Browser
import Css.Graph
import Css.View as Css
import FontAwesome
import Hex
import Hovercard
import Html
import Html.Attributes
import Html.Styled exposing (Attribute, Html, div, img, span, text)
import Html.Styled.Attributes exposing (classList, css, src, title, value)
import Html.Styled.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Switch
import Util.Css
import View.Locale as Locale


none : Html msg
none =
    span [] []


nona : Attribute msg
nona =
    classList []


aa : (a -> Attribute msg) -> Maybe a -> List (Attribute msg) -> List (Attribute msg)
aa toAttr value =
    (++) (value |> Maybe.map (toAttr >> List.singleton) |> Maybe.withDefault [])


toCssColor : Color.Color -> Color
toCssColor color =
    Color.toRgba color
        |> (\{ red, green, blue, alpha } ->
                Css.rgba (red * 255 |> Basics.round) (green * 255 |> Basics.round) (blue * 255 |> Basics.round) alpha
           )


loadingSpinner : View.Config -> (View.Config -> List Style) -> Html msg
loadingSpinner vc css_ =
    img
        [ src vc.theme.loadingSpinnerUrl
        , css_ vc |> css
        ]
        []


firstToUpper : String -> String
firstToUpper str =
    String.left 1 str
        |> String.toUpper
        |> (\f -> f ++ String.dropLeft 1 str)


truncate : Int -> String -> String
truncate len str =
    if String.length str > len && len > 6 then
        String.left (len - 3) str ++ "…"

    else
        str


truncateLongIdentifier : String -> String
truncateLongIdentifier str =
    if String.length str > 18 then
        let
            sigPart =
                if String.startsWith "0x" str then
                    String.right (String.length str - 2) str

                else
                    str

            len =
                8
        in
        String.left len sigPart ++ "…" ++ String.right len sigPart

    else
        str


setAlpha : Float -> Color.Color -> Color.Color
setAlpha alpha =
    Color.toRgba
        >> (\c -> { c | alpha = alpha })
        >> Color.fromRgba


hovercard : View.Config -> Hovercard.Model -> Int -> List (Html.Html msg) -> Html.Styled.Html msg
hovercard vc element zIndex =
    Hovercard.view
        { tickLength = 16
        , zIndex = zIndex
        , borderColor = (vc.theme.hovercard vc.lightmode).borderColor
        , backgroundColor = (vc.theme.hovercard vc.lightmode).backgroundColor
        , borderWidth = (vc.theme.hovercard vc.lightmode).borderWidth
        , viewport = vc.size
        }
        element
        (Css.hovercard vc
            |> List.map (\( k, v ) -> Html.Attributes.style k v)
        )
        >> Html.Styled.fromUnstyled


switch : View.Config -> List (Attribute msg) -> String -> Html msg
switch =
    switchInternal False


onOffSwitch : View.Config -> List (Attribute msg) -> String -> Html msg
onOffSwitch =
    switchInternal True


switchInternal : Bool -> View.Config -> List (Attribute msg) -> String -> Html msg
switchInternal showOnColor vc attrs title =
    div
        [ Css.switchRoot vc |> css
        ]
        [ Switch.switch 2 1 Css.rem
            |> Switch.duration 200
            |> Switch.onStyle
                (if showOnColor then
                    [ vc.theme.switchOnColor vc.lightmode
                        |> toCssColor
                        |> Css.backgroundColor
                    ]

                 else
                    []
                )
            |> Switch.offStyle
                [ (if vc.lightmode then
                    Css.rgba 0 0 0 0.2

                   else
                    Css.rgba 255 255 255 0.2
                  )
                    |> Css.backgroundColor
                ]
            |> Switch.knobStyle
                [ (if vc.lightmode then
                    Css.rgb 0 0 0

                   else
                    Css.rgb 255 255 255
                  )
                    |> Css.backgroundColor
                ]
            |> Switch.attributes attrs
            |> Switch.render
        , title
            |> text
            |> List.singleton
            |> span [ Css.switchLabel vc |> css ]
        ]


p : View.Config -> List (Attribute msg) -> List (Html msg) -> Html msg
p vc attrs =
    Html.Styled.p
        ((Css.paragraph vc |> css) :: attrs)


addDot : String -> String
addDot s =
    s ++ "."


contextMenuRule : View.Config -> List (Html msg)
contextMenuRule vc =
    [ Html.Styled.hr [ Css.Graph.contextMenuRule vc |> css ] [] ]


copyableLongIdentifier : View.Config -> List (Attribute msg) -> String -> Html msg
copyableLongIdentifier vc attr identifier =
    span
        [ Css.longIdentifier vc |> css
        ]
        [ text (truncateLongIdentifier identifier)
            |> List.singleton
            |> span
                (title identifier
                    :: attr
                )
        , copyIcon vc identifier
        ]


copyIcon : View.Config -> String -> Html msg
copyIcon =
    copyIconWithAttr []


copyIconWithAttr : List (Attribute msg) -> View.Config -> String -> Html msg
copyIconWithAttr attr vc value =
    Html.Styled.a
        ([ Css.copyIcon vc |> css
         , title (Locale.string vc.locale "copy")
         ]
            ++ attr
        )
        [ Html.Styled.node "copy-icon"
            [ Html.Styled.Attributes.attribute "data-value" value
            ]
            [ FontAwesome.icon FontAwesome.clone
                |> Html.Styled.fromUnstyled
            ]
        ]


longIdentifier : View.Config -> String -> Html msg
longIdentifier vc address =
    span
        [ Css.longIdentifier vc |> css
        ]
        [ text (truncateLongIdentifier address)
        ]


colorToHex : Color.Color -> String
colorToHex cl =
    let
        { red, green, blue } =
            Color.toRgba cl
    in
    List.map (round >> Hex.toString) [ red * 255, green * 255, blue * 255 ]
        |> List.map (String.padLeft 2 '0')
        |> (::) "#"
        |> String.join ""


frame : View.Config -> List (Attribute msg) -> List (Html msg) -> Html msg
frame vc attr =
    div
        ((Css.frame vc |> css) :: attr)
