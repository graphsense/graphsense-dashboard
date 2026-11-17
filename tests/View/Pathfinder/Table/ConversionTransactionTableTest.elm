module View.Pathfinder.Table.ConversionTransactionTableTest exposing (suite)

{-| The Bridge transactions section lists a conversion's two legs. Each leg's
hash can be copied and opened in its chain's explorer, so the panel above does
not repeat the legs.
-}

import Api.Data
import Css.Table
import Expect
import Html.Attributes
import Html.Styled
import Init.Pathfinder.ConversionDetails as ConversionDetails
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Graph.Table
import View.Pathfinder.Table.ConversionTransactionTable as ConversionTransactionTable


sendHash : String
sendHash =
    "507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0"


payoutHash : String
payoutHash =
    "cfa572bc24099007fdcef331b17d30e83673b7c6972ec1c04419afba9b58a720"


leg : String -> String -> String -> Api.Data.Tx
leg network hash identifier =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "0xdac17f958d2ee523a2206206994597c13d831ec7"
        , fee = Nothing
        , fromAddress = "0x1c1df1eb43bb46e1f6e1a4d59bd15ddbf7a0cdaf"
        , height = 20000000
        , identifier = identifier
        , isExternal = Nothing
        , network = network
        , timestamp = 1718000000
        , toAddress = "0x0a0c1a9cbaef41e1e5a5b4e34d4b2a49f8b7e2b1"
        , tokenTxId = Nothing
        , txHash = hash
        , txType = "account"
        , value = { fiatValues = [], value = 1000000 }
        }


{-| The USDT0 transfer arb 0x507fa262… → eth 0xcfa572bc….
-}
usdt0 : Api.Data.ExternalConversion
usdt0 =
    { conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
    , fromAddress = "0x1c1df1eb43bb46e1f6e1a4d59bd15ddbf7a0cdaf"
    , fromAmount = "1000000"
    , fromAsset = "0xfd086bc7cd5c481dcc9c85ebe478a1c0b69fcbb9"
    , fromAssetTransfer = sendHash ++ "_T13"
    , fromIsSupportedAsset = True
    , fromNetwork = "arb"
    , toAddress = "0x1c1df1eb43bb46e1f6e1a4d59bd15ddbf7a0cdaf"
    , toAmount = "1000000"
    , toAsset = "0xdac17f958d2ee523a2206206994597c13d831ec7"
    , toAssetTransfer = payoutHash ++ "_T658"
    , toIsSupportedAsset = True
    , toNetwork = "eth"
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Nothing
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Nothing
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    , protocol = Just "layerzero"
    , protocolApp = Just "usdt0"
    , transferId = Nothing
    }


legTable : Query.Single ()
legTable =
    let
        legIds =
            ( Id.init "arb" usdt0.fromAssetTransfer, Id.init "eth" usdt0.toAssetTransfer )

        model =
            ConversionEdge.init usdt0
                legIds
                ( Id.init "arb" usdt0.fromAddress, Id.init "eth" usdt0.toAddress )
                (leg "arb" sendHash usdt0.fromAssetTransfer)
                (leg "eth" payoutHash usdt0.toAssetTransfer)
                |> ConversionDetails.init
    in
    View.Graph.Table.table Css.Table.styles
        Env.viewConfig
        []
        (ConversionTransactionTable.config Css.Table.styles Env.viewConfig usdt0.conversionType legIds (always False))
        model.table
        |> Html.Styled.map (always ())
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


suite : Test
suite =
    describe "View.Pathfinder.Table.ConversionTransactionTable"
        [ test "every leg link opens its chain's explorer in a new tab and sends no referrer" <|
            \_ ->
                legTable
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Html.Attributes.href ("https://arbiscan.io/tx/0x" ++ sendHash)) ]
                        , Query.has [ Selector.attribute (Html.Attributes.href ("https://etherscan.io/tx/0x" ++ payoutHash ++ "/advanced#eventlog#658")) ]
                        , Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-leg-explorer-link") ]
                            >> Expect.all
                                [ Query.count (Expect.equal 2)
                                , Query.each (Query.has [ Selector.attribute (Html.Attributes.target "_blank") ])
                                , Query.each (Query.has [ Selector.attribute (Html.Attributes.rel "noopener noreferrer") ])
                                ]
                        ]
        ]
