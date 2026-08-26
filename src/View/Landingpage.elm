module View.Landingpage exposing (view)

import Config.View as View
import Css exposing (hover)
import Css.Transitions exposing (transition)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (autofocus, css)
import Html.Styled.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode
import Model exposing (Model, Msg(..))
import Model.Search
import Msg.Pathfinder as Pathfinder
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Buttons as Buttons
import Theme.Html.Landingpage as Landingpage
import Theme.Html.SettingsComponents as Sc
import Util.View exposing (pointer)
import View.Button as Button
import View.Locale as Locale
import View.Search



--import Css exposing (marginRight)


{-| Succeeds only if the drag carries files (not text selections etc.).
-}
whenDraggingFiles : Msg -> Json.Decode.Decoder Msg
whenDraggingFiles msg =
    Json.Decode.at [ "dataTransfer", "types" ] (Json.Decode.list Json.Decode.string)
        |> Json.Decode.andThen
            (\types ->
                if List.member "Files" types then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "not dragging files"
            )


searchBoxView : View.Config -> Model.Search.Model -> Html Msg
searchBoxView vc model =
    Sc.searchBarFieldStateTypingWithInstances
        (Sc.searchBarFieldStateTypingAttributes
            |> Rs.s_root
                [ Util.View.testId "gs-landing-search"
                , css
                    [ Css.alignItems Css.stretch |> Css.important
                    , Css.rem 23 |> Css.width |> Css.important
                    ]
                ]
        )
        (Sc.searchBarFieldStateTypingInstances
            |> Rs.s_searchInputField
                (View.Search.searchWithMoreCss
                    vc
                    (View.Search.default
                        |> Rs.s_inputAttributes [ autofocus True ]
                    )
                    model
                    |> Html.Styled.map SearchMsg
                    |> Just
                )
        )
        {}


view : View.Config -> Model key -> Html Msg
view vc model =
    Landingpage.startInvestigationWithInstances
        (Landingpage.startInvestigationAttributes
            |> Rs.s_root
                [ css [ Css.overflow Css.visible ] ]
            |> Rs.s_openGsFileFrame
                [ onClick (PathfinderMsg Pathfinder.UserClickedOpenGraph)
                , pointer
                , css
                    [ hover [ Css.property "background-color" Colors.grey50 ]
                    , transition [ Css.Transitions.backgroundColor 100 ]
                    , Css.pointerEvents Css.none
                    ]
                , [ Css.property "transition" "transform 0.15s ease-out"
                  , Css.transform
                        (if model.fileDragOver then
                            Css.scale 1.1

                         else
                            Css.scale 1
                        )
                  ]
                    |> css

                -- dragover must be cancelled for the box to be a drop target
                , preventDefaultOn "dragover" (Json.Decode.succeed ( NoOp, True ))
                , on "dragenter" (whenDraggingFiles (UserDraggedFileOverLoadBox True))
                , on "dragleave" (Json.Decode.succeed (UserDraggedFileOverLoadBox False))

                -- some drag sources advertise files at dragenter but deliver none at
                -- drop; still reset the drag-over state then, or the box stays enlarged
                , preventDefaultOn "drop"
                    (Json.Decode.at [ "dataTransfer", "files", "0" ] Json.Decode.value
                        |> Json.Decode.maybe
                        |> Json.Decode.map
                            (\file ->
                                ( file
                                    |> Maybe.map UserDroppedFileOnLoadBox
                                    |> Maybe.withDefault (UserDraggedFileOverLoadBox False)
                                , True
                                )
                            )
                    )
                ]
        )
        Landingpage.startInvestigationInstances
        { examplesList =
            [ ( "1Archive1n2C579dMsAu3iC6tWzuQJz8dN", "address" )
            , ( "8c510d39be9458721bdde62f64b096812de23c0ebd37a4aff82b8abb6307beb6", "transaction" )
            ]
                |> List.map
                    (\( str, name ) ->
                        Button.button vc
                            (Button.defaultConfig
                                |> Rs.s_size Buttons.ButtonSizeSmall
                                |> Rs.s_onClick (Just <| UserClickedExampleSearch str)
                                |> Rs.s_onClickWithStop True
                                |> Rs.s_text (name |> Locale.string vc.locale)
                                |> Rs.s_style Buttons.ButtonStyleTextGrey
                            )
                    )
        }
        { searchBar = { variant = searchBoxView vc model.search }
        , root =
            { title = Locale.string vc.locale "Landingpage-start-new"
            , alternativesTitle = Locale.string vc.locale "or"
            , examplesTitle = Locale.string vc.locale "Landingpage-try-example" ++ ":"
            , openGsFileTitle = Locale.string vc.locale "Landingpage-load-file"
            }
        }
        |> List.singleton
        |> div
            [ css
                [ Css.width <| Css.pct 100
                , Css.height <| Css.pct 100
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.flexStart
                , Css.paddingTop <| Css.px 200
                ]
            ]
