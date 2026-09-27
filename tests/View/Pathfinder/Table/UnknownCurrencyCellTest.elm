module View.Pathfinder.Table.UnknownCurrencyCellTest exposing (suite)

{-| A transfer of a token the dashboard has no metadata for (a plain uncurated
row in Sub Transfers) cannot be formatted: no decimals, no symbol. The cell
says "unknown currency"; clicking it copies the contract address and hovering
it shows that address.
-}

import Api.Data
import Expect
import Html.Attributes
import Html.Styled exposing (div, toUnstyled)
import Model.Currency exposing (AssetIdentifier)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Pathfinder.Table.Columns as Columns


unknownContract : String
unknownContract =
    "0x2c3a8ee94ddd97244a93bc48298f97d2c412f7db"


cell : AssetIdentifier -> Query.Single msg
cell asset =
    (Columns.assetsCell Env.viewConfig False False False [ ( asset, values ) ]).children
        |> div []
        |> toUnstyled
        |> Query.fromHtml


values : Api.Data.Values
values =
    { value = -4127208515966861312, fiatValues = [] }


suite : Test
suite =
    describe "a value cell for a token without metadata"
        [ test "reads unknown currency, without the address in the text" <|
            \_ ->
                cell { network = "bnb", asset = unknownContract }
                    |> Query.find [ Selector.tag "copy-icon" ]
                    |> Query.has [ Selector.containing [ Selector.text "unknown currency" ] ]
        , test "copies the contract address on click" <|
            \_ ->
                cell { network = "bnb", asset = unknownContract }
                    |> Query.find [ Selector.tag "copy-icon" ]
                    |> Query.has [ Selector.attribute (Html.Attributes.attribute "data-value" unknownContract) ]
        , test "shows the contract address on hover" <|
            \_ ->
                cell { network = "bnb", asset = unknownContract }
                    |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-hint" "") ]
                    |> Query.has [ Selector.text unknownContract ]
        , test "a known token stays a plain value" <|
            \_ ->
                cell { network = "eth", asset = "eth" }
                    |> Query.findAll [ Selector.tag "copy-icon" ]
                    |> Query.count (Expect.equal 0)
        ]
