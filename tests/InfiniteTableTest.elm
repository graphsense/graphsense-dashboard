module InfiniteTableTest exposing (suite)

import Components.InfiniteTable as InfiniteTable
import Expect
import Html.Styled
import Table
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


config : InfiniteTable.Config () ()
config =
    { fetch = \_ _ _ -> ()
    , force = False
    , effectToTracker = \_ -> Nothing
    , abort = \_ -> ()
    , triggerOffset = 100
    }


tableConfig : InfiniteTable.TableConfig String InfiniteTable.Msg
tableConfig =
    { toId = identity
    , columns = [ Table.stringColumn "col" identity ]
    , customizations = Table.defaultCustomizations
    , tag = identity
    , loadingPlaceholderAbove = [ Html.Styled.text "SPINNER_ABOVE" ]
    , loadingPlaceholderBelow = [ Html.Styled.text "SPINNER_BELOW" ]
    }


suite : Test
suite =
    describe "InfiniteTable loading placeholder"
        [ test "is loading after loadFirstPage" <|
            \_ ->
                InfiniteTable.init "test-table" 10
                    |> InfiniteTable.loadFirstPage config
                    |> Tuple.first
                    |> InfiniteTable.isLoading
                    |> Expect.equal True
        , test "shows loading placeholder while first page is loading" <|
            \_ ->
                let
                    ( model, _ ) =
                        InfiniteTable.init "test-table" 10
                            |> InfiniteTable.loadFirstPage config
                in
                InfiniteTable.view tableConfig [] model
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "SPINNER_BELOW" ]
        ]
