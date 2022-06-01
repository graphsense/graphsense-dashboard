module Main exposing (main)

import Browser exposing (document)
import Browser.Dom as Dom
import Color
import Hovercard exposing (hovercard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task


type Msg
    = GotElement (Result Dom.Error Dom.Element)
    | ClickElement String


main : Program () (Maybe Dom.Element) Msg
main =
    document
        { init = \_ -> ( Nothing, Cmd.none )
        , view =
            \model ->
                { title = "Hovercard"
                , body =
                    [ div
                        [ style "width" "100vw"
                        , style "height" "100vh"
                        , style "display" "flex"
                        , style "flex-direction" "column"
                        , style "justify-content" "space-between"
                        ]
                        [ div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "space-between"
                            ]
                            [ div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "lefttop")
                                , id "lefttop"
                                ]
                                []
                            , div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "righttop")
                                , id "righttop"
                                ]
                                []
                            ]
                        , div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            ]
                            [ div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "middle")
                                , id "middle"
                                ]
                                []
                            ]
                        , div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            ]
                            [ div
                                [ style "width" "100%"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "long")
                                , id "long"
                                ]
                                []
                            ]
                        , div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "space-between"
                            ]
                            [ div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "leftbottom")
                                , id "leftbottom"
                                ]
                                []
                            , div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "rightbottom")
                                , id "rightbottom"
                                ]
                                []
                            ]
                        ]
                    ]
                        ++ (model
                                |> Maybe.map
                                    (\element ->
                                        [ hovercard
                                            { maxWidth = 100
                                            , maxHeight = 100
                                            , tickLength = 16
                                            , borderColor = Color.black
                                            , backgroundColor = Color.lightBlue
                                            , borderWidth = 2
                                            }
                                            -- Browser.Dom.Element representing
                                            -- viewport and position of the element
                                            element
                                            [ style "box-shadow" "5px 5px 5px 0px rgba(0,0,0,0.25)"
                                            ]
                                            [ div
                                                []
                                                [ text "Lorem ipsum dolor sit amet"
                                                ]
                                            ]
                                        ]
                                    )
                                |> Maybe.withDefault []
                           )
                }
        , update =
            \msg model ->
                case msg of
                    ClickElement id ->
                        ( model
                        , Dom.getElement id
                            |> Task.attempt GotElement
                        )

                    GotElement result ->
                        Result.toMaybe result
                            |> (\m -> ( m, Cmd.none ))
        , subscriptions = always Sub.none
        }
