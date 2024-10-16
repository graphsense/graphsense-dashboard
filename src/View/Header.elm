module View.Header exposing (HeaderConfig, header)

import Config.View exposing (Config)
import Css
import Css.Header as Css
import Css.Pathfinder exposing (searchInputStyle)
import Css.Search
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id, src)
import Model exposing (Msg(..), UserModel)
import Model.Search as Search
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import RecordSetter exposing (..)
import Theme.Html.SettingsComponents as SettingsComponents
import Util.View as View
import View.Search as Search
import View.User as User


type alias HeaderConfig =
    { search : Search.Model
    , user : UserModel
    , hideSearch : Bool
    }


header : Plugins -> ModelState -> Config -> HeaderConfig -> Html Msg
header plugins states vc hc =
    Html.Styled.header
        [ css
            [ Css.position Css.absolute
            , Css.displayFlex
            , Css.position Css.absolute
            , Css.zIndex (Css.int 1)
            , Css.px 40 |> Css.top
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.width (Css.pct 100)
            , Css.justifyContent Css.spaceAround
            ]
        , id "header"
        ]
        [ if hc.hideSearch then
            View.none

          else
            SettingsComponents.toolbarSearchFieldWithInstances
                (SettingsComponents.toolbarSearchFieldAttributes
                    |> s_toolbarSearchField
                        [ css [ Css.alignItems Css.stretch |> Css.important ] ]
                )
                (SettingsComponents.toolbarSearchFieldInstances
                    |> s_searchInputField
                        (Search.searchWithMoreCss plugins
                            vc
                            (Search.default
                                |> s_css (searchInputStyle vc)
                                |> s_formCss
                                    [ Css.flexGrow <| Css.num 1
                                    , Css.height Css.auto |> Css.important
                                    ]
                                |> s_frameCss
                                    [ Css.height <| Css.pct 100
                                    , Css.marginRight Css.zero |> Css.important
                                    ]
                                |> s_multiline True
                                |> s_resultsAsLink True
                                |> s_showIcon False
                            )
                            hc.search
                            |> Html.Styled.map SearchMsg
                            |> Just
                        )
                )
                {}
        ]
