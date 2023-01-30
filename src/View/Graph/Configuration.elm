module View.Graph.Configuration exposing (..)

import Config.Graph as Graph
import Config.View as View
import Css.Graph as Css
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency as Currency
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
                    , vc.locale.currency == Currency.Coin |> selected
                    ]
                    [ Locale.string vc.locale "Coin"
                        |> text
                    ]
                , option
                    [ value "eur"
                    , vc.locale.currency == Currency.Fiat "eur" |> selected
                    ]
                    [ text "EUR"
                    ]
                , option
                    [ value "usd"
                    , vc.locale.currency == Currency.Fiat "usd" |> selected
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
                    [ value "tag"
                    , config.addressLabelType == Graph.Tag |> selected
                    ]
                    [ Locale.string vc.locale "Label"
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
        ]
    ]
