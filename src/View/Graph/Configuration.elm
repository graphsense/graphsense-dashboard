module View.Graph.Configuration exposing (..)

import Config.Graph as Graph
import Config.View as View
import Css.Graph as Css
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Msg.Graph exposing (Msg(..))
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
                    ]
                    [ Locale.string vc.locale "Coin"
                        |> text
                    ]
                , option
                    [ value "eur"
                    ]
                    [ text "EUR"
                    ]
                , option
                    [ value "usd"
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
        ]
    ]
