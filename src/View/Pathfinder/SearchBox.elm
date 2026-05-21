module View.Pathfinder.SearchBox exposing (view)

import Config.View as View
import Css
import Html.Styled exposing (Html, button, div, input, span, text)
import Html.Styled.Attributes as Attr exposing (css, id, placeholder, type_, value)
import Html.Styled.Events exposing (onClick, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import Model.Pathfinder.SearchBox as SearchBox
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.SearchBox as SearchBoxMsg
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import View.Locale as Locale


view : View.Config -> SearchBox.Model -> Html Msg
view vc model =
    if not model.visible then
        text ""

    else
        div
            [ css containerStyle
            , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            , stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
            ]
            [ div [ css iconSlotStyle ] [ Icons.iconsSearchLarge {} ]
            , input
                [ id SearchBox.inputId
                , type_ "text"
                , value model.query
                , placeholder (Locale.string vc.locale "Search on graph")
                , Attr.autofocus True
                , onInput (SearchBoxMsg.UserChangedQuery >> OnGraphSearchMsg)
                , preventDefaultOn "keydown" keyDecoder
                , css inputStyle
                ]
                []
            , matchCounter vc model
            , navButton (OnGraphSearchMsg SearchBoxMsg.UserClickedPrev)
                "Previous match"
                chevronUp
            , navButton (OnGraphSearchMsg SearchBoxMsg.UserClickedNext)
                "Next match"
                chevronDown
            , navButton (OnGraphSearchMsg SearchBoxMsg.UserClickedClose)
                "Close"
                (Icons.iconsCloseBlack {})
            ]


matchCounter : View.Config -> SearchBox.Model -> Html Msg
matchCounter vc model =
    let
        total =
            List.length model.matches

        current =
            model.currentMatchIndex |> Maybe.map ((+) 1) |> Maybe.withDefault 0

        label =
            if String.isEmpty (String.trim model.query) then
                ""

            else if total == 0 then
                Locale.string vc.locale "No matches"

            else
                String.fromInt current ++ " / " ++ String.fromInt total
    in
    span [ css counterStyle ] [ text label ]


navButton : Msg -> String -> Html Msg -> Html Msg
navButton msg label icon =
    button
        [ onClick msg
        , Attr.title label
        , Attr.attribute "aria-label" label
        , css navButtonStyle
        ]
        [ icon ]


chevronDown : Html Msg
chevronDown =
    Icons.iconsChevronDownThin {}


chevronUp : Html Msg
chevronUp =
    div [ css [ Css.transform (Css.rotate (Css.deg 180)), Css.lineHeight (Css.num 0) ] ]
        [ Icons.iconsChevronDownThin {} ]


keyDecoder : Decode.Decoder ( Msg, Bool )
keyDecoder =
    Decode.map2
        (\key shift ->
            case key of
                "Escape" ->
                    Just ( OnGraphSearchMsg SearchBoxMsg.UserClickedClose, True )

                "Enter" ->
                    if shift then
                        Just ( OnGraphSearchMsg SearchBoxMsg.UserClickedPrev, True )

                    else
                        Just ( OnGraphSearchMsg SearchBoxMsg.UserPressedEnterInBox, True )

                _ ->
                    Nothing
        )
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        |> Decode.andThen
            (\maybeMsg ->
                case maybeMsg of
                    Just m ->
                        Decode.succeed m

                    Nothing ->
                        Decode.fail "ignored"
            )


containerStyle : List Css.Style
containerStyle =
    [ Css.position Css.absolute
    , Css.bottom (Css.px 17)
    , Css.left (Css.px 12)
    , Css.zIndex (Css.int 1000)
    , Css.displayFlex
    , Css.alignItems Css.center
    , Css.height (Css.px 30)
    , Css.boxSizing Css.borderBox
    , Css.property "background-color" Colors.white
    , Css.borderRadius (Css.px 5)
    , Css.property "border" ("1px solid " ++ Colors.greyBlue100)
    , Css.property "box-shadow" "0 1px 4px rgba(0, 0, 0, 0.12)"
    , Css.paddingLeft (Css.px 3)
    , Css.paddingRight (Css.px 4)
    , Css.property "gap" "2px"
    ]


iconSlotStyle : List Css.Style
iconSlotStyle =
    [ Css.displayFlex
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.flexShrink (Css.num 0)
    , Css.lineHeight (Css.num 0)
    ]


inputStyle : List Css.Style
inputStyle =
    [ Css.border Css.zero
    , Css.outline Css.none
    , Css.width (Css.px 180)
    , Css.minWidth Css.zero
    , Css.height (Css.px 28)
    , Css.padding2 Css.zero (Css.px 4)
    , Css.property "background-color" "transparent"
    , Css.fontFamilies [ "Roboto" ]
    , Css.fontSize (Css.px 12)
    , Css.fontWeight (Css.int 400)
    , Css.letterSpacing (Css.px 0)
    , Css.property "color" Colors.sidebarNeutral
    , Css.pseudoClass "placeholder"
        [ Css.property "color" Colors.greyBlue200
        ]
    ]


counterStyle : List Css.Style
counterStyle =
    [ Css.fontSize (Css.px 11)
    , Css.fontFamilies [ "Roboto" ]
    , Css.property "color" Colors.greyBlue200
    , Css.minWidth (Css.px 44)
    , Css.textAlign Css.center
    , Css.paddingLeft (Css.px 4)
    ]


navButtonStyle : List Css.Style
navButtonStyle =
    [ Css.border Css.zero
    , Css.property "background-color" "transparent"
    , Css.cursor Css.pointer
    , Css.padding (Css.px 2)
    , Css.displayFlex
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.borderRadius (Css.px 3)
    , Css.hover [ Css.property "background-color" Colors.greyBlue100 ]
    ]
