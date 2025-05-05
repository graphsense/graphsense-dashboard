module View.Header exposing (HeaderConfig, header)

import Config.View exposing (Config)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import Model exposing (Msg(..), UserModel)
import Model.Search as Search
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.SettingsComponents as SettingsComponents
import Util.View as View
import View.Search as Search


type alias HeaderConfig =
    { search : Search.Model
    , user : UserModel
    , hideSearch : Bool
    }


header : Plugins -> ModelState -> Config -> HeaderConfig -> Html Msg
header plugins _ vc hc =
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
                        (Search.searchWithMoreCss plugins
                            vc
                            (Search.default
                                |> Rs.s_css
                                    (\_ ->
                                        Css.outline Css.none
                                            :: Css.pseudoClass "placeholder" SettingsComponents.searchBarFieldStatePlaceholderSearchInputField_details.styles
                                            :: (Css.width <| Css.pct 100)
                                            :: SettingsComponents.searchBarFieldStateTypingSearchInputField_details.styles
                                            ++ SettingsComponents.searchBarFieldStateTypingSearchText_details.styles
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
                                |> Rs.s_multiline True
                                |> Rs.s_resultsAsLink True
                                |> Rs.s_showIcon False
                            )
                            hc.search
                            |> Html.Styled.map SearchMsg
                            |> Just
                        )
                )
                {}
        ]
