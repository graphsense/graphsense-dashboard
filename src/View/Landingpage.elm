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
import Model exposing (Model, Msg(..), getLatestBlocks)
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
                { searchable =
                    Search.SearchAll
                        { pluginStates = model.plugins
                        , latestBlocks = getLatestBlocks model.stats
                        }
                , css =
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
        , rule
            (if vc.lightmode then
                vc.theme.landingpage.ruleColor.light

             else
                vc.theme.landingpage.ruleColor.dark
            )
            [ CssLanding.rule vc |> css
            ]
            [ Locale.string vc.locale "or try these examples" |> text
            ]
        , div [ CssLanding.exampleLinkBox vc |> css ]
            [ a [ Css.View.link vc |> css, href "/graph/btc/address/1Archive1n2C579dMsAu3iC6tWzuQJz8dN" ] [ text "1Archi...QJz8dN" ]
            , text " / "
            , a [ Css.View.link vc |> css, href "/graph/btc/tx/8c510d39be9458721bdde62f64b096812de23c0ebd37a4aff82b8abb6307beb6" ] [ text "8c510d...07beb6" ]
            , text " / "
            , a [ Css.View.link vc |> css, href "/graph/label/internet%20archive" ] [ text "internet archive" ]
            , text " / "
            , a [ Css.View.link vc |> css, href "/graph/btc/block/1" ] [ text "1" ]
            , text " / "
            , a [ Css.View.link vc |> css, href "/graph/actor/internet_archive" ] [ text "Internet Archive" ]
            ]
        ]
