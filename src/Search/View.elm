module Search.View exposing (search)

import Api.Data
import Css exposing (block, display, none)
import FontAwesome
import Heroicons.Solid as Heroicons
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Locale.View as Locale
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Search.Css as Css
import Search.Init as Search
import Search.Model exposing (Model)
import Search.Msg exposing (Msg(..))
import Util.RemoteData exposing (webdata)
import View.Config exposing (Config)
import View.Css as Css


type alias SearchConfig =
    { latestBlocks : List ( String, Int )
    }


search : Config -> SearchConfig -> Model -> Html Msg
search vc sc model =
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
                , Locale.string vc.locale "Addresses, transaction, label, block" |> placeholder
                , onInput UserInputsSearch
                , value model.input
                ]
                []
            , searchResult vc sc model
            ]
        , button
            [ Css.primary vc |> css
            ]
            [ FontAwesome.icon FontAwesome.search
                |> Html.Styled.fromUnstyled
            ]
        ]


loadingSpinner : Config -> Bool -> Html Msg
loadingSpinner vc show =
    if show then
        img
            [ src vc.theme.loadingSpinnerUrl
            , Css.loadingSpinner vc |> css
            ]
            []

    else
        span [] []


searchResult : Config -> SearchConfig -> Model -> Html Msg
searchResult vc sc model =
    let
        rl =
            resultList vc sc model
    in
    if String.length model.input < 4 || List.isEmpty rl then
        span [] []

    else
        div
            [ id "search-result"
            , css (Css.result vc)
            ]
            (loadingSpinner vc model.loading
                :: rl
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


resultList : Config -> SearchConfig -> Model -> List (Html Msg)
resultList vc sc { found, input } =
    let
        filtered =
            Maybe.map (filterByPrefix input) found
    in
    List.map (currencyToResult vc input filtered) sc.latestBlocks
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


resultLineToHtml : Config -> String -> ResultLine -> Html Msg
resultLineToHtml vc title resultLine =
    let
        currency =
            String.toLower title

        ( route, icon, label ) =
            case resultLine of
                Address a ->
                    ( Route.Address { currency = currency, address = a }, FontAwesome.at, a )

                Tx a ->
                    ( Route.Tx { currency = currency, tx = a }, FontAwesome.exchangeAlt, a )

                Block a ->
                    ( Route.Block { currency = currency, block = a }, FontAwesome.cube, String.fromInt a )

                Label a ->
                    ( Route.Label a, FontAwesome.tag, a )
    in
    a
        [ Route.toUrl route
            |> href
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
