module View.Search exposing (Searchable(..), search)

import Api.Data
import Config.View exposing (Config)
import Css exposing (Style, block, display, none)
import Css.Search as Css
import Css.View
import FontAwesome
import Heroicons.Solid as Heroicons
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Init.Search as Search
import Model.Search exposing (..)
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
    { searchable : Searchable
    , css : List Style
    , resultsAsLink : Bool
    , multiline : Bool
    , showIcon : Bool
    }


type Searchable
    = SearchAll
        { latestBlocks : List ( String, Int )
        , pluginStates : PluginStates
        }
    | SearchTagsOnly


search : Plugins -> Config -> SearchConfig -> Model -> Html Msg
search plugins vc sc model =
    Html.Styled.form
        [ Css.form vc |> css
        ]
        [ div
            [ Css.frame vc |> css
            ]
            [ (if sc.multiline then
                textarea

               else
                input
              )
                [ sc.css |> css
                , autocomplete False
                , spellcheck False
                , Locale.string vc.locale "The search" |> title
                , case sc.searchable of
                    SearchAll _ ->
                        [ "Addresses", "transaction", "label", "block" ]
                            |> List.map (Locale.string vc.locale)
                            |> (\st -> st ++ Plugin.View.Search.placeholder plugins vc)
                            |> String.join ", "
                            |> placeholder

                    SearchTagsOnly ->
                        Locale.string vc.locale "Label"
                            |> placeholder
                , onInput UserInputsSearch
                , value model.input
                ]
                []
            , searchResult plugins vc sc model
            ]
        , if sc.showIcon then
            button
                [ Css.View.primary vc |> css
                ]
                [ FontAwesome.icon FontAwesome.search
                    |> Html.Styled.fromUnstyled
                ]

          else
            Util.View.none
        ]


searchResult : Plugins -> Config -> SearchConfig -> Model -> Html Msg
searchResult plugins vc sc model =
    let
        rl =
            resultList plugins vc sc model
    in
    if not model.loading && List.isEmpty rl then
        span [] []

    else
        div
            [ id "search-result"
            , css (Css.result vc)
            , onClick UserClicksResult
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


resultList : Plugins -> Config -> SearchConfig -> Model -> List (Html Msg)
resultList plugins vc sc { found, input } =
    let
        filtered =
            Maybe.map (filterByPrefix input) found

        labelBadge =
            { title = Locale.string vc.locale "Labels"
            , badge =
                Maybe.map .labels found
                    |> Maybe.withDefault []
                    |> List.map Label
            }

        badgeToResult { title, badge } =
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
                    , List.map (resultLineToHtml vc title sc.resultsAsLink) badge
                        |> ol [ Css.resultGroupList vc |> css ]
                    ]
                    |> Just
    in
    case sc.searchable of
        SearchTagsOnly ->
            [ labelBadge ]
                |> List.filterMap badgeToResult

        SearchAll { latestBlocks, pluginStates } ->
            (List.map (currencyToResult vc input filtered) latestBlocks
                ++ [ labelBadge
                   ]
                |> List.filterMap badgeToResult
            )
                ++ Plugin.View.Search.resultList plugins pluginStates vc


resultLineToHtml : Config -> String -> Bool -> ResultLine -> Html Msg
resultLineToHtml vc title asLink resultLine =
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

        el attr =
            if asLink then
                a ((Route.graphRoute route |> toUrl |> href) :: attr)

            else
                div attr
    in
    el
        [ Css.resultLine vc |> css
        , onClick (UserClicksResultLine resultLine)
        ]
        [ FontAwesome.icon icon
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span [ Css.resultLineIcon vc |> css ]
        , text label
        ]


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
