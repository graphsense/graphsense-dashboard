module View.Graph.Table.LinksTable exposing (..)

import Api.Data
import Config.View as View
import Css
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Table as T exposing (Table)
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.View exposing (truncate)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale
import View.Util exposing (copyableLongIdentifier)


init : Table String
init =
    Init.Graph.Table.initSorted True filter "url"


filter : String -> String -> Bool
filter f a =
    String.contains f a

config : View.Config -> Table.Config String Msg
config vc =
    Table.customConfig
        { toId = \data -> data
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                "Url"
                identity
                (\data -> [
                    text data
                        |> List.singleton
                        |> a
                            [ href data
                            , target "_blank"
                            , Css.View.link vc |> css
                            ]
                    ]
                )
            ]
        , customizations = customizations vc
        }
