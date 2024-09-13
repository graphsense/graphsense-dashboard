module View.Header exposing (header)

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
            , Css.zIndex (Css.int 20)
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
                            { css = searchInputStyle vc
                            , formCss =
                                Just
                                    [ Css.flexGrow <| Css.num 1
                                    , Css.height Css.auto |> Css.important
                                    ]
                            , frameCss =
                                Just
                                    [ Css.height <| Css.pct 100
                                    , Css.marginRight Css.zero |> Css.important
                                    ]
                            , multiline = True
                            , resultsAsLink = True
                            , showIcon = True
                            }
                            hc.search
                            |> Html.Styled.map SearchMsg
                            |> Just
                        )
                )
                {}
        ]


headerOld : Plugins -> ModelState -> Config -> HeaderConfig -> Html Msg
headerOld plugins states vc hc =
    Html.Styled.header
        [ Css.header vc |> css
        , id "header"
        ]
        [ logo vc
        , if hc.hideSearch then
            View.none

          else
            Search.search plugins
                vc
                { css = Css.Search.textarea vc
                , resultsAsLink = True
                , multiline = True
                , showIcon = True
                }
                hc.search
                |> Html.Styled.map SearchMsg
        , User.user vc hc.user
        ]


logo : Config -> Html Msg
logo vc =
    div
        [ Css.headerLogoWrap vc |> css ]
        [ img
            [ src <|
                if vc.lightmode then
                    vc.theme.logo_lightmode

                else
                    vc.theme.logo
            , Css.headerLogo vc |> css
            ]
            []
        ]
