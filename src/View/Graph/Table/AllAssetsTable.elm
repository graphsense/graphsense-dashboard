module View.Graph.Table.AllAssetsTable exposing (..)

import Api.Data
import Config.View as View
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.Table exposing (titleCurrency, titleValue)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Table
import Tuple exposing (first, second)
import Util.Csv
import Util.View exposing (copyableLongIdentifier)
import View.Graph.Table as T exposing (customizations)


config : View.Config -> Table.Config ( String, Api.Data.Values ) Msg
config vc =
    Table.customConfig
        { toId = first
        , toMsg = TableNewState
        , columns =
            [ T.stringColumn vc titleCurrency (first >> String.toUpper)
            , T.valueColumnWithoutCode vc first titleValue second
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> ( String, Api.Data.Values ) -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( ( titleCurrency, [] ), Util.Csv.string <| first row )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "value" (second row) locModel currency
