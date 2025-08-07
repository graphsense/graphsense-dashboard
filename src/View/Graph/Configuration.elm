module View.Graph.Configuration exposing (configuration)

import Config.Graph as Graph
import Config.View as View
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Util.View exposing (onOffSwitch)
import View.Dialog as Dialog
import View.Locale as Locale


configuration : View.Config -> Graph.Config -> List (Html Msg)
configuration vc config =
    [ div
        [ onMouseLeave (UserClicksConfiguraton "")
        ]
        [ Dialog.part vc
            "Change currency"
            [ select
                [ Css.View.input vc |> css
                , onInput UserChangesCurrency
                ]
                [ option
                    [ value "coin"
                    , vc.showValuesInFiat |> not |> selected
                    ]
                    [ Locale.string vc.locale "Coin"
                        |> text
                    ]
                , option
                    [ value "eur"
                    , vc.showValuesInFiat && vc.preferredFiatCurrency == "eur" |> selected
                    ]
                    [ text "EUR"
                    ]
                , option
                    [ value "usd"
                    , vc.showValuesInFiat && vc.preferredFiatCurrency == "usd" |> selected
                    ]
                    [ text "USD"
                    ]
                ]
            ]
        , Dialog.part vc
            "Address label"
            [ select
                [ Css.View.input vc |> css
                , onInput UserChangesAddressLabelType
                ]
                [ option
                    [ value "id"
                    , config.addressLabelType == Graph.ID |> selected
                    ]
                    [ Locale.string vc.locale "ID"
                        |> text
                    ]
                , option
                    [ value "balance"
                    , config.addressLabelType == Graph.Balance |> selected
                    ]
                    [ Locale.string vc.locale "Balance"
                        |> text
                    ]
                , option
                    [ value "total received"
                    , config.addressLabelType == Graph.TotalReceived |> selected
                    ]
                    [ Locale.string vc.locale "Total received"
                        |> text
                    ]
                , option
                    [ value "tag"
                    , config.addressLabelType == Graph.Tag |> selected
                    ]
                    [ Locale.string vc.locale "Label"
                        |> text
                    ]
                ]
            ]
        , Dialog.part vc
            "Value format"
            [ select
                [ Css.View.input vc |> css
                , onInput UserChangesValueDetail
                ]
                [ option
                    [ value "exact"
                    , vc.locale.valueDetail == Model.Locale.Exact |> selected
                    ]
                    [ Locale.string vc.locale "Exact"
                        |> text
                    ]
                , option
                    [ value "magnitude"
                    , vc.locale.valueDetail == Model.Locale.Magnitude |> selected
                    ]
                    [ Locale.string vc.locale "Magnitude"
                        |> text
                    ]
                ]
            ]
        , Dialog.part vc
            "Transaction label"
            [ select
                [ Css.View.input vc |> css
                , onInput UserChangesTxLabelType
                ]
                [ option
                    [ value "notxs"
                    , config.txLabelType == Graph.NoTxs |> selected
                    ]
                    [ Locale.string vc.locale "No. transactions"
                        |> text
                    ]
                , option
                    [ value "value"
                    , config.txLabelType == Graph.Value |> selected
                    ]
                    [ Locale.string vc.locale "Value"
                        |> text
                    ]
                ]
            ]
        , Dialog.part vc
            "Show shadow links"
            [ Locale.string vc.locale "for entities"
                |> onOffSwitch vc
                    [ checked config.showEntityShadowLinks
                    , onClick UserClickedShowEntityShadowLinks
                    ]
            , Locale.string vc.locale "for addresses"
                |> onOffSwitch vc
                    [ checked config.showAddressShadowLinks
                    , onClick UserClickedShowAddressShadowLinks
                    ]
            ]
        , Dialog.part vc
            "Show zero value transactions"
            [ onOffSwitch vc
                [ checked config.showZeroTransactions
                , onClick UserClickedToggleShowZeroTransactions
                ]
                ""
            ]
        , Dialog.part vc
            "Timezone"
            [ Locale.string vc.locale
                (if vc.showDatesInUserLocale then
                    "User"

                 else
                    "UTC"
                )
                |> onOffSwitch vc
                    [ checked vc.showDatesInUserLocale
                    , onClick UserClickedToggleShowDatesInUserLocale
                    ]
            ]
        ]
    ]
