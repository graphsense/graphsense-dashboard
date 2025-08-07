module View.Graph.InfiniteTable exposing (Config, table)

import Config.View as View
import Css.Table exposing (Styles)
import Html.Styled exposing (Html)
import Html.Styled.Events exposing (stopPropagationOn)
import Json.Decode
import Model.Pathfinder.InfiniteTable exposing (InfiniteTable)
import Table
import View.Graph.Table


type alias Config msg =
    { scrollMsg : ScrollPos -> msg
    }


table : Styles -> View.Config -> Config msg -> List (Attribute msg) -> Table.Config data msg -> InfiniteTable data -> Html msg
table styles vc config _ =
    View.Graph.Table.table styles
        vc
        [ stopPropagationOn "scroll" (Json.Decode.map (\pos -> ( config.scrollMsg pos, True )) decodeScrollPos)
        ]
        { filter = Nothing
        , csv = Nothing
        }
