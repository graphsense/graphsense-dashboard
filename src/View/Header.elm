module View.Header exposing (HeaderConfig, header)

import Config.View exposing (Config)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import Model exposing (Msg(..), UserModel)
import Model.Search as Search
import Plugin.Model exposing (ModelState)
import RecordSetter as Rs
import Theme.Html.SettingsComponents as SettingsComponents
import Util.View as View
import View.Search as Search


type alias HeaderConfig =
    { search : Search.Model
    , user : UserModel
    , hideSearch : Bool
    }


header : ModelState -> Config -> HeaderConfig -> Html Msg
header _ vc hc =
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
            SettingsComponents.searchBarFieldStateTypingWithInstances
                (SettingsComponents.searchBarFieldStateTypingAttributes
                    |> Rs.s_root
                        [ css
                            [ Css.alignItems Css.stretch |> Css.important
                            , Css.px 325 |> Css.width |> Css.important
                            ]
                        ]
                )
                (SettingsComponents.searchBarFieldStateTypingInstances
                    |> Rs.s_searchInputField
                        (Search.searchWithMoreCss
                            vc
                            (Search.default
                                |> Rs.s_multiline True
                            )
                            hc.search
                            |> Html.Styled.map SearchMsg
                            |> Just
                        )
                )
                {}
        ]
