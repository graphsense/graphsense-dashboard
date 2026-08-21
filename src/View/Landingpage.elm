module View.Landingpage exposing (view)

import Config.View as View
import Css
import Css.Landingpage as CssLanding
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Model exposing (Model, Msg(..))
import Model.Search
import Msg.Pathfinder as Pathfinder
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.SettingsComponents as Sc
import Util.View
import Util.View.Rule exposing (rule)
import View.Locale as Locale
import View.Search



--import Css exposing (marginRight)


frame : View.Config -> List (Html Msg) -> Html Msg
frame vc =
    div
        [ CssLanding.frame vc |> css
        ]
        >> List.singleton
        >> div
            [ CssLanding.root vc |> css
            ]


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
                        |> Rs.s_css
                            (\_ ->
                                Css.outline Css.none
                                    :: Css.pseudoClass "placeholder" Sc.searchBarFieldStatePlaceholderSearchInputField_details.styles
                                    :: (Css.width <| Css.pct 100)
                                    :: Sc.searchBarFieldStateTypingSearchInputField_details.styles
                                    ++ Sc.searchBarFieldStateTypingSearchText_details.styles
                            )
                        |> Rs.s_formCss
                            [ Css.flexGrow <| Css.num 1
                            , Css.height Css.auto |> Css.important
                            ]
                        |> Rs.s_frameCss
                            [ Css.height <| Css.pct 100
                            , Css.marginRight Css.zero |> Css.important
                            ]
                        |> Rs.s_resultLine
                            [ Css.property "background-color" Colors.white
                            , Css.hover
                                [ Css.property "background-color" Colors.greyBlue50
                                    |> Css.important
                                ]
                            ]
                        |> Rs.s_resultLineHighlighted
                            [ Css.property "background-color" Colors.greyBlue50
                            ]
                        |> Rs.s_resultsAsLink True
                        |> Rs.s_dropdownResult
                            [ Css.property "background-color" Colors.white
                            ]
                        |> Rs.s_dropdownFrame
                            [ Css.property "background-color" Colors.white
                            ]
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
    frame vc
        [ h2
            [ Css.View.heading2 vc |> css
            ]
            [ Locale.text vc.locale "Landingpage-start-new"
            ]
        , searchBoxView vc model.search
            |> List.singleton
            |> div
                [ CssLanding.searchRoot vc |> css
                ]
        , [ ( "1Archive1n2C579dMsAu3iC6tWzuQJz8dN", "address" )
          , ( "8c510d39be9458721bdde62f64b096812de23c0ebd37a4aff82b8abb6307beb6", "transaction" )

          --   , ( "internet archive", "label" )
          --   , ( "123", "block" )
          ]
            |> List.map
                (\( str, name ) ->
                    span
                        [ Css.View.link vc |> css
                        , ( UserClickedExampleSearch str, True )
                            |> Json.Decode.succeed
                            |> stopPropagationOn "click"
                        ]
                        [ name |> Locale.string vc.locale |> text
                        ]
                )
            |> List.intersperse (text " / ")
            |> (::) (Locale.string vc.locale "Landingpage-try-example" ++ ": " |> text)
            |> div [ CssLanding.exampleLinkBox vc |> css ]
        , rule
            (if vc.lightmode then
                vc.theme.landingpage.ruleColor.light

             else
                vc.theme.landingpage.ruleColor.dark
            )
            [ CssLanding.rule vc |> css
            ]
            [ Locale.string vc.locale "or" |> text
            ]
        , div
            [ CssLanding.loadBox vc
                ++ [ Css.property "transition" "transform 0.15s ease-out"
                   , Css.transform
                        (if model.fileDragOver then
                            Css.scale 1.1

                         else
                            Css.scale 1
                        )
                   ]
                |> css
            , onClick (PathfinderMsg Pathfinder.UserClickedOpenGraph)

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
            -- pointer-events none on the children, so dragleave only fires
            -- when the cursor leaves the box, not when it crosses a child
            [ div
                [ CssLanding.loadBoxIcon vc ++ [ Css.pointerEvents Css.none ] |> css
                ]
                [ FontAwesome.icon FontAwesome.folderOpen
                    |> Html.Styled.fromUnstyled
                ]
            , div
                [ CssLanding.loadBoxText vc ++ [ Css.pointerEvents Css.none ] |> css
                ]
                [ Locale.string vc.locale "Landingpage-load-file" |> text
                ]
            ]
        ]
