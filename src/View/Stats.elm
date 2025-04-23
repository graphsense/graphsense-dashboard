module View.Stats exposing (stats)

import Api.Data
import Config.View exposing (Config)
import Css.Stats as Css
import Css.View
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import RemoteData exposing (WebData)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as Svg exposing (d, viewBox)
import Util.RemoteData exposing (webdata)
import Util.View
import View.CurrencyMeta exposing (networks)
import View.Locale as Locale


stats : Config -> WebData Api.Data.Stats -> Dict String Api.Data.TokenConfigs -> Html msg
stats vc sts tokens =
    Util.View.frame vc
        []
        [ h2
            [ Css.View.heading2 vc |> css
            ]
            [ Locale.text vc.locale "Ledger statistics"
            ]
        , sts
            |> webdata
                { onFailure = statsLoadFailure
                , onNotAsked = text ""
                , onLoading = statsLoading vc
                , onSuccess = statsLoaded vc tokens
                }
        ]


statsLoadFailure : Http.Error -> Html msg
statsLoadFailure _ =
    text "error"


statsLoading : Config -> Html msg
statsLoading vc =
    Util.View.loadingSpinner vc Css.loadingSpinner


statsLoaded : Config -> Dict String Api.Data.TokenConfigs -> Api.Data.Stats -> Html msg
statsLoaded vc tokens sts =
    sts.currencies
        |> List.map (\v -> currency vc v (Dict.get v.name tokens))
        |> div
            [ Css.stats vc |> css ]


supportedTokens : Api.Data.TokenConfigs -> List String
supportedTokens configs =
    configs.tokenConfigs |> List.map (.ticker >> String.toUpper)


supportedTokensRow : Config -> Maybe Api.Data.TokenConfigs -> List (Html msg)
supportedTokensRow vc tokens =
    tokens |> Maybe.map (supportedTokens >> statsRowBadge vc "Supported tokens" >> List.singleton) |> Maybe.withDefault []


currency : Config -> Api.Data.CurrencyStats -> Maybe Api.Data.TokenConfigs -> Html msg
currency vc cs tokens =
    div
        [ Css.currency vc |> css
        ]
        [ h3
            [ Css.currencyHeading vc |> css
            ]
            [ Dict.get cs.name networks
                |> Maybe.map .name
                |> Maybe.withDefault (cs.name |> String.toUpper)
                |> text
            ]
        , div
            [ Css.statsTableWrapper vc |> css
            ]
            [ div
                [ Css.statsTableInnerWrapper vc |> css
                ]
                [ div
                    [ Css.statsTable vc |> css
                    ]
                    ([ Locale.timestamp vc.locale cs.timestamp
                        |> statsRow vc "Last update"
                     , Locale.intWithoutValueDetailFormatting vc.locale (cs.noBlocks - 1)
                        |> statsRow vc "Latest block"
                     , Locale.intWithoutValueDetailFormatting vc.locale cs.noTxs
                        |> statsRow vc "Transactions"
                     , Locale.intWithoutValueDetailFormatting vc.locale cs.noAddresses
                        |> statsRow vc "Addresses"
                     , Locale.intWithoutValueDetailFormatting vc.locale cs.noEntities
                        |> statsRow vc "Entities"
                     , Locale.intWithoutValueDetailFormatting vc.locale cs.noLabels
                        |> statsRow vc "Labels"
                     , taggedAddressesWithPercentage vc cs
                        |> statsRow vc "Tagged addresses"
                     ]
                        ++ supportedTokensRow vc tokens
                    )
                ]
            , div
                [ Css.currencyBackground vc |> css
                ]
                [ Dict.get cs.name networks
                    |> Maybe.map
                        (\{ icon } ->
                            svg
                                [ viewBox "0 0 100 100"
                                , attribute "height" "100%"
                                , attribute "width" "100%"
                                ]
                                [ path
                                    [ Css.currencyBackgroundPath vc |> Svg.css
                                    , d icon
                                    ]
                                    []
                                ]
                        )
                    |> Maybe.withDefault (span [] [])
                ]
            ]
        ]


statsRow : Config -> String -> String -> Html msg
statsRow vc label value =
    div
        [ Css.statsTableRow vc |> css
        ]
        [ span
            [ Css.statsTableCellKey vc |> css
            ]
            [ Locale.text vc.locale label
            ]
        , span
            [ Css.statsTableCellValue vc |> css
            ]
            [ text value
            ]
        ]


statsRowBadge : Config -> String -> List String -> Html msg
statsRowBadge vc label values =
    div
        [ Css.statsTableRow vc |> css
        ]
        [ span
            [ Css.statsTableCellKey vc |> css
            ]
            [ Locale.text vc.locale label
            ]
        , div
            [ Css.statsBadgeContainer |> css
            ]
            (values |> List.map (\x -> div [ Css.statsBadge vc |> css ] [ text x ]))
        ]


taggedAddressesWithPercentage : Config -> Api.Data.CurrencyStats -> String
taggedAddressesWithPercentage vc cs =
    Locale.intWithoutValueDetailFormatting vc.locale cs.noTaggedAddresses
        ++ " ("
        ++ Locale.percentage vc.locale
            (toFloat cs.noTaggedAddresses / toFloat cs.noAddresses)
        ++ ")"
