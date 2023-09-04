module View.Landingpage exposing (..)

import Config.View as View
import Css.Landingpage as Css
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
import Util.View.Rule exposing (rule)
import View.Locale as Locale
import View.Search as Search


frame : View.Config -> List (Html Msg) -> Html Msg
frame vc =
    div
        [ Css.frame vc |> css
        ]
        >> List.singleton
        >> div
            [ Css.root vc |> css
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
                [ Css.searchRoot vc |> css
                ]
        , rule
            (if vc.lightmode then
                vc.theme.landingpage.ruleColor.light

             else
                vc.theme.landingpage.ruleColor.dark
            )
            [ Css.rule vc |> css
            ]
            [ Locale.string vc.locale "or" |> text
            ]
        , div
            [ Css.loadBox vc |> css
            , onClick (GraphMsg Graph.UserClickedImportGS)
            ]
            [ div
                [ Css.loadBoxIcon vc |> css
                ]
                [ FontAwesome.icon FontAwesome.folderOpen
                    |> Html.Styled.fromUnstyled
                ]
            , div
                [ Css.loadBoxText vc |> css
                ]
                [ Locale.string vc.locale "Load graph from .gs file" |> text
                ]
            ]
        ]
