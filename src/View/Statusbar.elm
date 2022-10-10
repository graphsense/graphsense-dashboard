module View.Statusbar exposing (..)

import Api
import Config.View as View
import Css.Statusbar as Css
import Dict
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import List.Extra
import Model exposing (Msg(..))
import Model.Graph.Id as Id
import Model.Graph.Search as Search
import Model.Statusbar exposing (..)
import Util.View exposing (firstToUpper, loadingSpinner, none)
import Version exposing (version)
import View.Locale as Locale


view : View.Config -> Model -> Html Msg
view vc model =
    div
        ([ Css.root vc model.visible |> css
         ]
            ++ (if model.visible then
                    []

                else
                    [ onClick UserClickedStatusbar
                    ]
               )
        )
        (if not model.visible then
            [ model.messages
                |> Dict.values
                |> List.head
                |> Maybe.map (message vc)
                |> Maybe.withDefault none
            , "v"
                ++ version
                |> text
                |> List.singleton
                |> span []
            ]

         else
            button
                [ Css.close vc |> css
                , onClick UserClickedStatusbar
                ]
                [ FontAwesome.icon FontAwesome.times
                    |> Html.Styled.fromUnstyled
                ]
                :: (model.messages
                        |> Dict.values
                        |> List.map (message vc)
                        |> (\m ->
                                m
                                    ++ (model.log
                                            |> List.map (log vc)
                                       )
                           )
                   )
        )


message : View.Config -> ( String, List String ) -> Html Msg
message vc ( key, values ) =
    div
        [ Css.log vc True |> css
        ]
        [ loadingSpinner vc Css.loadingSpinner
        , messageString vc key values
        ]


messageString : View.Config -> String -> List String -> Html Msg
messageString vc key values =
    values
        |> List.map (Locale.string vc.locale)
        |> Locale.interpolated vc.locale (firstToUpper key)
        |> text
        |> List.singleton
        |> span []


log : View.Config -> ( String, List String, Maybe Http.Error ) -> Html Msg
log vc ( key, values, error ) =
    div
        [ Css.log vc (error == Nothing) |> css
        ]
        [ messageString vc key values
        , text <|
            case error of
                Just e ->
                    ": "
                        ++ (case e of
                                Http.BadUrl url ->
                                    Locale.string vc.locale "bad url" ++ " " ++ url

                                Http.Timeout ->
                                    Locale.string vc.locale "timeout"

                                Http.NetworkError ->
                                    Locale.string vc.locale "network error"

                                Http.BadStatus 500 ->
                                    Locale.string vc.locale "server error"

                                Http.BadStatus 429 ->
                                    Locale.string vc.locale "API rate limit exceeded. Please try again later."

                                Http.BadStatus 404 ->
                                    Locale.string vc.locale "not found"

                                Http.BadStatus 504 ->
                                    Locale.string vc.locale "timeout"
                                        ++ (if key == searchNeighborsKey then
                                                ". " ++ Locale.string vc.locale "Please try again with a lower depth/breadth setting."

                                            else
                                                ""
                                           )

                                Http.BadStatus s ->
                                    Locale.string vc.locale "bad status" ++ ": " ++ String.fromInt s

                                Http.BadBody str ->
                                    if str == Api.noExternalTransactions then
                                        Locale.string vc.locale str

                                    else
                                        Locale.string vc.locale "data error" ++ " " ++ str
                           )

                Nothing ->
                    ""
        ]
