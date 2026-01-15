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
import Msg.Graph as Graph
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.SettingsComponents as Sc
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


searchBoxView : Plugins -> View.Config -> Model.Search.Model -> Html Msg
searchBoxView plugins vc model =
    Sc.searchBarFieldStateTypingWithInstances
        (Sc.searchBarFieldStateTypingAttributes
            |> Rs.s_root
                [ css
                    [ Css.alignItems Css.stretch |> Css.important
                    , Css.rem 23 |> Css.width |> Css.important
                    ]
                ]
        )
        (Sc.searchBarFieldStateTypingInstances
            |> Rs.s_searchInputField
                (View.Search.searchWithMoreCss plugins
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
                    )
                    model
                    |> Html.Styled.map SearchMsg
                    |> Just
                )
        )
        {}


view : Plugins -> View.Config -> Model key -> Html Msg
view plugins vc model =
    frame vc
        [ h2
            [ Css.View.heading2 vc |> css
            ]
            [ Locale.text vc.locale "Landingpage-start-new"
            ]
        , searchBoxView plugins vc model.search
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
            [ CssLanding.loadBox vc |> css
            , onClick (GraphMsg Graph.UserClickedImportGS)
            ]
            [ div
                [ CssLanding.loadBoxIcon vc |> css
                ]
                [ FontAwesome.icon FontAwesome.folderOpen
                    |> Html.Styled.fromUnstyled
                ]
            , div
                [ CssLanding.loadBoxText vc |> css
                ]
                [ Locale.string vc.locale "Landingpage-load-file" |> text
                ]
            ]
        ]
