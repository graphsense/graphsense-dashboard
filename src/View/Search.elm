module View.Search exposing (search)

import Api.Data
import Config.View exposing (Config)
import Css exposing (block, display, none)
import Css.Search as Css
import Css.View as Css
import FontAwesome
import Heroicons.Solid as Heroicons
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Init.Search as Search
import Model.Search exposing (Model)
import Msg.Search exposing (Msg(..))
import Plugin exposing (Plugins)
import Plugin.Model exposing (PluginStates)
import Plugin.View.Search
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (toUrl)
import Route.Graph as Route exposing (Route)
import Util.RemoteData exposing (webdata)
import Util.View exposing (loadingSpinner)
import View.Locale as Locale


type alias SearchConfig =
    { latestBlocks : List ( String, Int )
    }


search : Plugins -> PluginStates -> Config -> SearchConfig -> Model -> Html Msg
search plugins states vc sc model =
    Html.Styled.form
        [ Css.form vc |> css
        ]
        [ div
            [ Css.frame vc |> css
            ]
            [ textarea
                [ Css.textarea vc |> css
                , autocomplete False
                , spellcheck False
                , Locale.string vc.locale "The search" |> title
                , [ "Addresses", "transaction", "label", "block" ]
                    |> List.map (Locale.string vc.locale)
                    |> (\st -> st ++ Plugin.View.Search.placeholder plugins vc)
                    |> String.join ", "
                    |> placeholder
                , onInput UserInputsSearch
                , value model.input
                ]
                []
            , searchResult plugins states vc sc model
            ]
        , button
            [ Css.primary vc |> css
            ]
            [ FontAwesome.icon FontAwesome.search
                |> Html.Styled.fromUnstyled
            ]
        ]


searchResult : Plugins -> PluginStates -> Config -> SearchConfig -> Model -> Html Msg
searchResult plugins states vc sc model =
    let
        rl =
            resultList plugins states vc sc model
    in
    if String.length model.input < 4 then
        span [] []

    else
        div
            [ id "search-result"
            , css (Css.result vc)
            , onClick UserClicksResultLine
            ]
            ((if model.loading then
                [ loadingSpinner vc Css.loadingSpinner ]

              else
                []
             )
                ++ rl
            )


filterByPrefix : String -> Api.Data.SearchResult -> Api.Data.SearchResult
filterByPrefix input result =
    { result
        | currencies =
            List.map
                (\currency ->
                    { currency
                        | addresses = List.filter (String.startsWith input) currency.addresses
                        , txs = List.filter (String.startsWith input) currency.txs
                    }
                )
                result.currencies
    }


resultList : Plugins -> PluginStates -> Config -> SearchConfig -> Model -> List (Html Msg)
resultList plugins states vc sc { found, input } =
    let
        filtered =
            Maybe.map (filterByPrefix input) found
    in
    (List.map (currencyToResult vc input filtered) sc.latestBlocks
        ++ [ { title = Locale.string vc.locale "Labels"
             , badge =
                Maybe.map .labels found
                    |> Maybe.withDefault []
                    |> List.map Label
             }
           ]
        |> List.filterMap
            (\{ title, badge } ->
                if List.isEmpty badge then
                    Nothing

                else
                    div
                        [ Css.resultGroup vc |> css
                        ]
                        [ div
                            [ Css.resultGroupTitle vc |> css
                            ]
                            [ text title
                            ]
                        , List.map (resultLineToHtml vc title) badge
                            |> ol [ Css.resultGroupList vc |> css ]
                        ]
                        |> Just
            )
    )
        ++ Plugin.View.Search.resultList plugins states vc


resultLineToHtml : Config -> String -> ResultLine -> Html Msg
resultLineToHtml vc title resultLine =
    let
        currency =
            String.toLower title

        ( route, icon, label ) =
            case resultLine of
                Address a ->
                    ( Route.addressRoute { currency = currency, address = a, table = Nothing, layer = Nothing }
                    , FontAwesome.at
                    , a
                    )

                Tx a ->
                    ( Route.Currency currency (Route.Tx a), FontAwesome.exchangeAlt, a )

                Block a ->
                    ( Route.Currency currency (Route.Block a), FontAwesome.cube, String.fromInt a )

                Label a ->
                    ( Route.Label a, FontAwesome.tag, a )
    in
    a
        [ Route.graphRoute route |> toUrl |> href
        , Css.resultLine vc |> css
        ]
        [ FontAwesome.icon icon
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span [ Css.resultLineIcon vc |> css ]
        , text label
        ]


type ResultLine
    = Address String
    | Tx String
    | Block Int
    | Label String


currencyToResult : Config -> String -> Maybe Api.Data.SearchResult -> ( String, Int ) -> { title : String, badge : List ResultLine }
currencyToResult vc input found ( currency, latestBlock ) =
    { title = String.toUpper currency
    , badge =
        (Maybe.map
            (\{ currencies } ->
                List.filter (.currency >> (==) currency) currencies
                    |> List.head
                    |> Maybe.map
                        (\{ addresses, txs } ->
                            List.map Address addresses
                                ++ List.map Tx txs
                        )
                    |> Maybe.withDefault []
            )
            found
            |> Maybe.withDefault []
        )
            ++ blocksToResult input latestBlock
    }


blocksToResult : String -> Int -> List ResultLine
blocksToResult input latestBlock =
    String.toInt input
        |> Maybe.andThen
            (\i ->
                if i >= 0 && i <= latestBlock then
                    Just [ Block i ]

                else
                    Nothing
            )
        |> Maybe.withDefault []
