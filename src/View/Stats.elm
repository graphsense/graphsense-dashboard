module View.Stats exposing (stats)

import Api.Data
import Config.View exposing (Config)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import List.Nonempty
import RemoteData exposing (WebData)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes exposing (d, viewBox)
import Theme.Html.Page as Page
import Theme.Html.Stats as Stats
import Util.Data as Data
import Util.RemoteData exposing (webdata)
import Util.View.Loadingspinner as Loadingspinner
import View.CurrencyMeta exposing (networks)
import View.Locale as Locale


stats : Config -> WebData Api.Data.Stats -> Dict String Api.Data.TokenConfigs -> Html msg
stats vc sts tokens =
    Page.pageWithTitleWithAttributes
        Page.pageWithTitleAttributes
        { root =
            { title = Locale.string vc.locale "Ledger statistics"
            , subtitle = ""
            , content =
                sts
                    |> webdata
                        { onFailure = statsLoadFailure vc
                        , onNotAsked = text ""
                        , onLoading = statsLoading
                        , onSuccess = statsLoaded vc tokens
                        }
            }
        }


statsLoadFailure : Config -> Http.Error -> Html msg
statsLoadFailure vc err =
    Locale.httpErrorToString vc.locale err
        |> text


statsLoading : Html msg
statsLoading =
    Loadingspinner.html []


statsLoaded : Config -> Dict String Api.Data.TokenConfigs -> Api.Data.Stats -> Html msg
statsLoaded vc tokens sts =
    Stats.networks
        { networkList =
            sts.currencies
                |> List.map (\v -> currency vc v (Dict.get v.name tokens))
        }
        {}


supportedTokens : Api.Data.TokenConfigs -> List String
supportedTokens configs =
    configs.tokenConfigs |> List.map (.ticker >> String.toUpper)


supportedTokensRow : Config -> Maybe Api.Data.TokenConfigs -> List (Html msg)
supportedTokensRow vc tokens =
    tokens
        |> Maybe.map supportedTokens
        |> Maybe.andThen (List.Nonempty.fromList >> Maybe.map List.Nonempty.toList)
        |> Maybe.map (statsRowBadge vc "Supported tokens" >> List.singleton)
        |> Maybe.withDefault []


currency : Config -> Api.Data.CurrencyStats -> Maybe Api.Data.TokenConfigs -> Html msg
currency vc cs tokens =
    Stats.network
        { dataRowList =
            [ Data.timestampToPosix cs.timestamp
                |> Locale.timestamp vc.locale
                |> statsRow vc "Last update"
            , Locale.intWithoutValueDetailFormatting vc.locale (cs.noBlocks - 1)
                |> statsRow vc "Latest block"
            , Locale.intWithoutValueDetailFormatting vc.locale cs.noTxs
                |> statsRow vc "transactions"
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
        }
        { root =
            { label =
                Dict.get cs.name networks
                    |> Maybe.map .name
                    |> Maybe.withDefault (cs.name |> String.toUpper)
            , backgroundImage =
                Dict.get cs.name networks
                    |> Maybe.map
                        (\{ icon } ->
                            svg
                                [ viewBox "0 0 100 100"
                                , attribute "height" "100%"
                                , attribute "width" "100%"
                                , attribute "max-height" "10rem"
                                ]
                                [ path
                                    [ d icon
                                    ]
                                    []
                                ]
                        )
                    |> Maybe.withDefault (span [] [])
            }
        }


statsRow : Config -> String -> String -> Html msg
statsRow vc label value =
    Stats.dataRow
        { root =
            { key = Locale.string vc.locale label
            , value = value
            }
        }


statsRowBadge : Config -> String -> List String -> Html msg
statsRowBadge vc label values =
    Stats.tokensRow
        { tokenList =
            values |> List.map (\x -> Stats.token { root = { label = x } })
        }
        { root =
            { key = Locale.string vc.locale label
            }
        }


taggedAddressesWithPercentage : Config -> Api.Data.CurrencyStats -> String
taggedAddressesWithPercentage vc cs =
    Locale.intWithoutValueDetailFormatting vc.locale cs.noTaggedAddresses
        ++ " ("
        ++ Locale.percentage vc.locale
            (toFloat cs.noTaggedAddresses / toFloat cs.noAddresses)
        ++ ")"
