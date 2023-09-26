module View.Landingpage exposing (..)

import Browser exposing (UrlRequest(..))
import Config.View as View
import Css exposing (..)
import Css.Landingpage as CssLanding
import Css.Search
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Model exposing (Model, Msg(..))
import Msg.Graph as Graph
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Url
import Util.View.Rule exposing (rule)
import View.Locale as Locale
import View.Search as Search



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


view : Plugins -> View.Config -> Model key -> Html Msg
view plugins vc model =
    frame vc
        [ h2
            [ Css.View.heading2 vc |> css
            ]
            [ Locale.text vc.locale "Start a new investigation"
            ]
        , let
            sc =
                { css =
                    Css.Search.textarea vc
                , resultsAsLink = True
                , multiline = True
                , showIcon = True
                }
          in
          Search.search plugins vc sc model.search
            |> Html.Styled.map SearchMsg
            |> List.singleton
            |> div
                [ CssLanding.searchRoot vc |> css
                ]
        , [ ( "1Archive1n2C579dMsAu3iC6tWzuQJz8dN", "address" )
          , ( "8c510d39be9458721bdde62f64b096812de23c0ebd37a4aff82b8abb6307beb6", "transaction" )
          , ( "internet archive", "label" )
          , ( "1", "block" )
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
            |> (::) (Locale.string vc.locale "Try an example" ++ ": " |> text)
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
                [ Locale.string vc.locale "Load graph from .gs file" |> text
                ]
            ]
        ]
