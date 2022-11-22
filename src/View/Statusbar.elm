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
import Tuple exposing (..)
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
                                            |> List.map (log vc model.lastBlocks)
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
            |> text
            |> List.singleton
            |> span []
        ]


messageString : View.Config -> String -> List String -> String
messageString vc key values =
    values
        |> List.map (Locale.string vc.locale)
        |> Locale.interpolated vc.locale (firstToUpper key)


log : View.Config -> List ( String, Int ) -> ( String, List String, Maybe Http.Error ) -> Html Msg
log vc lastBlocks ( key, values, error ) =
    div
        [ Css.log vc (error == Nothing) |> css
        ]
        [ text <|
            messageString vc key values
                ++ (case error of
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
                                            if key == loadingAddressKey then
                                                List.Extra.getAt 1 values
                                                    |> Maybe.andThen
                                                        (\curr ->
                                                            lastBlocks
                                                                |> List.Extra.find (first >> String.toUpper >> (==) (String.toUpper curr))
                                                                |> Maybe.map (second >> Locale.int vc.locale)
                                                        )
                                                    |> Maybe.map
                                                        (List.singleton
                                                            >> Locale.interpolated vc.locale "Cannot find this address. It is possibly not yet in our database (synced up to block {0}) or has not received any transactions and is therefore not stored in the blockchain."
                                                        )
                                                    |> Maybe.withDefault (Locale.string vc.locale "not found")

                                            else
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
                   )
        ]
