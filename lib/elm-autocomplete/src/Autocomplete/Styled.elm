module Autocomplete.Styled exposing
    ( Events, EventMapper
    , events
    , autocomplete
    )

{-| Autocomplete.Styled exposes [HTML.Styled](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css) events to be attached for input and every autocomplete choice.


# Type

@docs Events, EventMapper


# Attributes

@docs events

-}

import Css
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Internal exposing (KeyDown(..), Msg(..))
import Json.Decode as JD


{-| Record to hold the events to be attached for input and every autocomplete choice
-}
type alias Events msg =
    { inputEvents : List (Attribute msg)
    , choiceEvents : Int -> List (Attribute msg)
    }


{-| Map Autocomplete Msg into your app's msg and also the msg to send when user selects a choice
-}
type alias EventMapper msg =
    { onSelect : msg
    , mapHtml : Msg -> msg
    }


{-| Returns the events to be attached for input and every autocomplete choice
-}
events : EventMapper msg -> Events msg
events mapper =
    { inputEvents = inputEvents mapper
    , choiceEvents = choiceEvents mapper
    }


inputEvents : EventMapper msg -> List (Attribute msg)
inputEvents mapper =
    let
        { mapHtml } =
            mapper
    in
    [ Events.onInput (mapHtml << OnInput)
    , Events.onBlur (mapHtml <| OnBlur)
    , Events.onFocus (mapHtml <| OnFocus)
    , Events.preventDefaultOn "keydown" <| onKeyDownDecoder mapper
    ]


choiceEvents : EventMapper msg -> Int -> List (Attribute msg)
choiceEvents mapper index =
    let
        { onSelect, mapHtml } =
            mapper
    in
    -- We cannot use onClick twice
    -- so we use onMouseDown/Up to send to Autocomplete first
    -- and then onClick will send to user's app
    -- onMouseDown/Up will always fire before onClick
    -- See https://www.w3schools.com/jsref/event_onmouseup.asp
    [ Events.preventDefaultOn "mousedown" <| JD.succeed ( mapHtml <| OnMouseDown index, True )
    , Events.onMouseUp <| mapHtml <| OnMouseUp index
    , Events.onClick onSelect
    ]


onKeyDownDecoder : EventMapper msg -> JD.Decoder ( msg, Bool )
onKeyDownDecoder mapper =
    let
        { onSelect, mapHtml } =
            mapper
    in
    JD.field "key" JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "ArrowUp" ->
                        JD.succeed ( mapHtml <| OnKeyDown ArrowUp, True )

                    "ArrowDown" ->
                        JD.succeed ( mapHtml <| OnKeyDown ArrowDown, True )

                    "Enter" ->
                        JD.succeed ( onSelect, True )

                    _ ->
                        JD.fail "Ignore other keys"
            )


autocomplete : List (Attribute msg) -> { input : Html msg, result : Html msg, loadingSpinner : Html msg, visible : Bool } -> Html msg
autocomplete attr { input, result, loadingSpinner, visible } =
    div
        (css [ Css.position Css.relative ]
            :: attr
        )
        [ input
        , if not visible then
            text ""

          else
            div
                [ css
                    [ Css.width <| Css.pct 100
                    , Css.position Css.absolute
                    , Css.zIndex <| Css.int 1
                    ]
                ]
                [ div
                    [ css
                        [ Css.position Css.absolute
                        , Css.right <| Css.px 5
                        , Css.top <| Css.px 5
                        ]
                    ]
                    [ loadingSpinner
                    ]
                , result
                ]
        ]
