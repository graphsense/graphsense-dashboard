module SearchUppercaseBech32Test exposing (suite)

{-| BIP173 allows a bech32 address in all uppercase too (QR codes use it).
The API finds it and answers with the lowercase form it stores, so the search
box must compare that answer with the normalized query too. A base58 address
can start the same way by chance (LTC: always "L", then random characters, so
"LTC1…"), and the API finds it as typed, so the typed query must still count.
-}

import Api.Data
import Autocomplete
import Expect
import Html.Styled
import Init.Search
import Msg.Search
import RecordSetter as Rs
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Search
import View.Search


address : String
address =
    "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"


{-| A base58 LTC address that starts with "LTC1" by chance.
-}
ltcBase58Address : String
ltcBase58Address =
    "LTC1A4GhnLkKbpfkaDyXHTeShP2YksUVgY"


apiAnswer : String -> String -> Api.Data.SearchResult
apiAnswer currency found =
    { currencies = [ { currency = currency, addresses = [ found ], txs = [] } ]
    , labels = []
    , actors = Nothing
    }


{-| The dropdown after the API answered `query`, as the app has it: the answer
only counts while the box is fetching.
-}
dropdown : Api.Data.SearchResult -> String -> Query.Single Msg.Search.Msg
dropdown answer query =
    Init.Search.init (Init.Search.initSearchAddressAndTxs Nothing)
        |> Rs.s_visible True
        |> (\search -> Rs.s_autocomplete (search.autocomplete |> Autocomplete.setQuery query |> Autocomplete.setStatus Autocomplete.Fetching) search)
        |> Update.Search.update (Msg.Search.BrowserGotSearchResult query answer)
        |> Tuple.first
        |> View.Search.searchWithMoreCss Env.viewConfig View.Search.default
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


suite : Test
suite =
    describe "searching for an uppercase bech32 address"
        [ test "keeps the lowercase address the API answers with" <|
            \_ ->
                Update.Search.filterByPrefix "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN" (apiAnswer "btc" address)
                    |> .currencies
                    |> List.concatMap .addresses
                    |> Expect.equal [ address ]
        , test "shows the typed part of the result in regular text, as for a lowercase query" <|
            \_ ->
                dropdown (apiAnswer "btc" address) "bc1qx8hy"
                    |> Query.has [ Selector.exactText "bc1qx8hy" ]
        , test "shows the typed part of the result in regular text" <|
            \_ ->
                dropdown (apiAnswer "btc" address) "BC1QX8HY"
                    |> Query.has [ Selector.exactText "bc1qx8hy" ]
        , test "keeps a base58 address that starts like it by chance" <|
            \_ ->
                Update.Search.filterByPrefix "LTC1A4G" (apiAnswer "ltc" ltcBase58Address)
                    |> .currencies
                    |> List.concatMap .addresses
                    |> Expect.equal [ ltcBase58Address ]
        , test "shows the typed part of that base58 address in regular text" <|
            \_ ->
                dropdown (apiAnswer "ltc" ltcBase58Address) "LTC1A4G"
                    |> Query.has [ Selector.exactText "LTC1A4G" ]
        ]
