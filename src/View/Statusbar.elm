module View.Statusbar exposing (view)

import Api
import Config.View as View
import Css as CSS
import Css.Statusbar as Css
import Dict
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import List.Extra
import Model exposing (Msg(..))
import Model.Statusbar exposing (..)
import Tuple exposing (..)
import Util.View exposing (firstToUpper, loadingSpinner, none)
import Version exposing (version)
import View.Locale as Locale


view : View.Config -> Model -> Html Msg
view vc model =
    div
        ((Css.root vc model.visible |> css)
            :: (if model.visible then
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
            , version
                |> text
                |> List.singleton
                |> span [ [ CSS.paddingRight (CSS.px 5) ] |> css ]
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
        [ (if error == Nothing then
            FontAwesome.check

           else
            FontAwesome.times
          )
            |> FontAwesome.icon
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span [ Css.logIcon vc (error == Nothing) |> css ]
        , text <|
            messageString vc key values
                ++ (case error of
                        Just e ->
                            ": "
                                ++ (case e of
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
                                                            >> Locale.interpolated vc.locale "Statusbar-not-found-info"
                                                        )
                                                    |> Maybe.withDefault (Locale.string vc.locale "not found")

                                            else
                                                Locale.httpErrorToString vc.locale e

                                        Http.BadStatus 504 ->
                                            Locale.httpErrorToString vc.locale e
                                                ++ (if key == searchNeighborsKey then
                                                        ". " ++ Locale.string vc.locale "Please try again with a lower depth/breadth setting."

                                                    else
                                                        ""
                                                   )

                                        Http.BadBody str ->
                                            if str == Api.noExternalTransactions then
                                                Locale.string vc.locale str

                                            else
                                                Locale.httpErrorToString vc.locale e

                                        _ ->
                                            Locale.httpErrorToString vc.locale e
                                   )

                        Nothing ->
                            ""
                   )
        ]
