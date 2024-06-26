module View.Graph.Table.LinksTable exposing (..)

import Config.View as View
import Css.Table exposing (styles)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.LinksTable exposing (..)
import Msg.Graph exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations)


config : View.Config -> Table.Config String Msg
config vc =
    Table.customConfig
        { toId = \data -> data
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
                titleUrl
                identity
                (\data ->
                    [ text data
                        |> List.singleton
                        |> a
                            [ href data
                            , target "_blank"
                            , Css.View.link vc |> css
                            ]
                    ]
                )
            ]
        , customizations = customizations styles vc
        }
