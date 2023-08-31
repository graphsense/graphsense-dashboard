module View.Graph.Landingpage exposing (..)

import Config.View as View
import Css.Landingpage as Css
import Css.Search
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..), getLatestBlocks)
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import View.Locale as Locale
import View.Search as Search


landingPage : Plugins -> View.Config -> Model key -> Html Msg
landingPage plugins vc model =
    div
        [ Css.root vc |> css
        ]
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
                , css = Css.Search.textarea vc
                , resultsAsLink = True
                , multiline = True
                , showIcon = True
                }
          in
          Search.search plugins vc sc model.search
            |> Html.Styled.map SearchMsg
        ]
