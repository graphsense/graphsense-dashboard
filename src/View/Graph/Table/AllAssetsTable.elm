module View.Graph.Table.AllAssetsTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css.Table exposing (styles)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency exposing (AssetIdentifier)
import Model.Graph.Table exposing (titleCurrency, titleValue)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Table
import Tuple exposing (first, second)
import Util.Csv
import View.Graph.Table as T exposing (customizations)


config : View.Config -> Table.Config ( AssetIdentifier, Api.Data.Values ) Msg
config vc =
    Table.customConfig
        { toId = first >> .asset
        , toMsg = TableNewState
        , columns =
            [ T.stringColumn styles vc titleCurrency (first >> .asset >> String.toUpper)
            , T.valueColumnWithoutCode styles vc first titleValue second
            ]
        , customizations = customizations styles vc
        }


prepareCSV : Model.Locale.Model -> String -> ( AssetIdentifier, Api.Data.Values ) -> List ( String, String )
prepareCSV locModel _ row =
    ( titleCurrency, Util.Csv.string <| (first row).asset )
        :: Util.Csv.valuesWithBaseCurrencyFloat "value" (second row) locModel (first row)
