module Scenario.PathfinderCsvExportTest exposing (suite)

{-| Regression cover for the transaction table's CSV export.

The utxo-only filter has no server-side equivalent: its rows come from the
`WorkflowNextUtxoTx` chain walk, whose responses are routed to the table rather
than to the message the caller hands in. The export used to re-run that fetch,
so its continuation was dropped, no data ever reached `Components.ExportCSV` and
the spinner turned forever.

-}

import Api.Data
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.TransactionFilter as TransactionFilter
import Effect.Api
import Effect.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Init.Pathfinder.Tx as Tx
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Time



-- FIXTURES


addressId : Id
addressId =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


{-| Runs `f` with the decoded fixtures, or fails with a clear message.
-}
withFixtures : (Api.Data.TxUtxo -> Api.Data.AddressTxUtxo -> Expectation) -> Expectation
withFixtures f =
    case
        Result.map2 f
            (Json.Decode.decodeString Api.Data.txUtxoDecoder Fixture.txUtxo)
            (Json.Decode.decodeString Api.Data.addressTxUtxoDecoder Fixture.addressTxUtxo)
    of
        Ok expectation ->
            expectation

        Err error ->
            Expect.fail ("a fixture did not decode: " ++ Json.Decode.errorToString error)



-- SETUP


{-| A transaction table holding `row`, filtered the way selecting a utxo quick
filter leaves it — `utxoOnly` is on by default, see `initSettingsModel`.
-}
tableWithUtxoOnlyFilter : Api.Data.TxUtxo -> Api.Data.AddressTxUtxo -> TransactionTable.Model
tableWithUtxoOnlyFilter tx row_ =
    let
        quickFilter =
            Tx.fromTxUtxoData 0 tx { x = 0, y = 0 }
                |> TransactionTable.quickFilterFromTx Outgoing
    in
    { table = tableHolding (rowOf tx row_)
    , order = Nothing
    , filter =
        TransactionFilter.initSettingsFromQuickFilter quickFilter
            |> TransactionFilter.init
            |> TransactionFilter.withQuickFilter quickFilter
    , maxChangeHopsLimit = Nothing
    }


{-| The same table with no filter at all — the ordinary, server-paged case.
-}
tableWithoutFilter : Api.Data.AddressTxUtxo -> TransactionTable.Model
tableWithoutFilter row =
    { table = tableHolding row
    , order = Nothing
    , filter =
        TransactionFilter.initSettings
            |> TransactionFilter.withDirection Nothing
            |> TransactionFilter.init
    , maxChangeHopsLimit = Nothing
    }


{-| The fixtures are two independent spec examples; pointing the row at the
transaction is what makes them one dataset the export can merge.
-}
rowOf : Api.Data.TxUtxo -> Api.Data.AddressTxUtxo -> Api.Data.AddressTxUtxo
rowOf tx row =
    { row | txHash = tx.txHash, currency = tx.currency }


tableHolding : Api.Data.AddressTxUtxo -> InfiniteTable.Model String Api.Data.AddressTx
tableHolding row =
    InfiniteTable.init "transactionTable" 25
        |> InfiniteTable.appendData InfiniteTable.config
            TransactionTable.filter
            Nothing
            [ Api.Data.AddressTxAddressTxUtxo row ]
        |> (\( table, _, _ ) -> table)


{-| The same table after a filter that matched nothing.
-}
withoutRows : TransactionTable.Model -> TransactionTable.Model
withoutRows table =
    { table | table = InfiniteTable.init "transactionTable" 25 }


{-| What clicking the export icon eventually dispatches: the click only asks for
the time, `BrowserGotTime` is the step that starts the fetch.
-}
clickExport : TransactionTable.Model -> App
clickExport table =
    App.init
        |> App.step
            (AddressDetails.ExportCSVMsg table (ExportCSV.BrowserGotTime (Time.millisToPosix 0))
                |> AddressDetailsMsg addressId
            )


{-| Carries the export past the two bulk lookups the utxo path needs: the full
transactions behind the rows, then the tag summaries of the addresses in them.
-}
runToCompletion : Api.Data.TxUtxo -> App -> App
runToCompletion tx app =
    app
        |> drain
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.BulkGetTxEffect _ toMsg ->
                        Just (toMsg [ ( tx.txHash, Api.Data.TxTxUtxo tx ) ])

                    _ ->
                        Nothing
            )
        |> drain
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.BulkGetAddressTagSummaryEffect _ toMsg ->
                        Just (toMsg [])

                    _ ->
                        Nothing
            )
        |> drain


{-| Feeds back whatever the last step asked the shell to re-dispatch.
-}
drain : App -> App
drain app =
    App.steps (App.internalMsgs app) app



-- ASSERTIONS


{-| True when the effect hands the table's rows straight to the export handler.
-}
isExportOfRows : List Api.Data.AddressTx -> Effect.Pathfinder.Effect -> Bool
isExportOfRows expected eff =
    case eff of
        Effect.Pathfinder.InternalEffect (AddressDetailsMsg _ (AddressDetails.GotAddressTxsForExport _ data)) ->
            data.addressTxs == expected

        Effect.Pathfinder.BatchEffect batched ->
            List.any (isExportOfRows expected) batched

        _ ->
            False


isNotification : Effect.Pathfinder.Effect -> Bool
isNotification eff =
    case eff of
        Effect.Pathfinder.ShowNotificationEffect _ ->
            True

        Effect.Pathfinder.BatchEffect batched ->
            List.any isNotification batched

        _ ->
            False


isAddressTxsRequest : Effect.Pathfinder.Effect -> Bool
isAddressTxsRequest eff =
    case eff of
        Effect.Pathfinder.ApiEffect (Effect.Api.GetAddressTxsByDateEffect _ _) ->
            True

        Effect.Pathfinder.BatchEffect batched ->
            List.any isAddressTxsRequest batched

        _ ->
            False



-- SCENARIOS


suite : Test
suite =
    describe "exporting the address transaction table as CSV"
        [ test "the utxo-only filter is what the fixtures produce" <|
            \_ ->
                withFixtures
                    (\tx row ->
                        tableWithUtxoOnlyFilter tx row
                            |> .filter
                            |> TransactionFilter.getUtxoFilter
                            |> (/=) Nothing
                            |> Expect.equal True
                            |> Expect.onFail "the setup no longer produces an active utxo-only filter"
                    )
        , test "with the utxo-only filter set, exports the rows the table holds" <|
            \_ ->
                withFixtures
                    (\tx row ->
                        tableWithUtxoOnlyFilter tx row
                            |> clickExport
                            |> App.expectEffect
                                "the table's rows handed to GotAddressTxsForExport"
                                (isExportOfRows [ Api.Data.AddressTxAddressTxUtxo (rowOf tx row) ])
                    )
        , test "with the utxo-only filter set, the download finishes" <|
            \_ ->
                withFixtures
                    (\tx row ->
                        tableWithUtxoOnlyFilter tx row
                            |> clickExport
                            |> runToCompletion tx
                            |> App.model
                            |> .exportCSV
                            |> ExportCSV.isDownloading
                            |> Expect.equal False
                            |> Expect.onFail "the export never received its data, so the spinner keeps turning"
                    )
        , test "with the utxo-only filter set, reports the download" <|
            \_ ->
                withFixtures
                    (\tx row ->
                        tableWithUtxoOnlyFilter tx row
                            |> clickExport
                            |> runToCompletion tx
                            |> App.expectEffect "a download notification" isNotification
                    )
        , test "an empty table finishes the download instead of hanging" <|
            \_ ->
                withFixtures
                    (\tx row ->
                        tableWithUtxoOnlyFilter tx row
                            |> withoutRows
                            |> clickExport
                            |> runToCompletion tx
                            |> App.model
                            |> .exportCSV
                            |> ExportCSV.isDownloading
                            |> Expect.equal False
                            |> Expect.onFail "with no rows there are no tags to fetch, so nothing ever completed the export"
                    )
        , test "without a filter, still asks the API for the rows" <|
            \_ ->
                withFixtures
                    (\_ row ->
                        tableWithoutFilter row
                            |> clickExport
                            |> App.expectEffect "a GetAddressTxsByDateEffect" isAddressTxsRequest
                    )
        ]
