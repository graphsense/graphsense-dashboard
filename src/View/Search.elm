module View.Search exposing (Searchable(..), search)

import Api.Data
import Config.View exposing (Config)
import Css exposing (Style, block, display, none)
import Css.Button
import Css.Search as Css
import Css.View
import FontAwesome
import Heroicons.Solid as Heroicons
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Search as Search
import Json.Decode
import Model.Search exposing (..)
import Msg.Search exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (toUrl)
import Route.Graph as Route exposing (Route)
import Util.RemoteData exposing (webdata)
import Util.View exposing (loadingSpinner, truncate)
import View.Autocomplete as Autocomplete
import View.Locale as Locale


type alias SearchConfig =
    { searchable : Searchable
    , css : String -> List Style
    , resultsAsLink : Bool
    , multiline : Bool
    , showIcon : Bool
    }


type Searchable
    = SearchAll
        { latestBlocks : List ( String, Int )
        , pluginStates : ModelState
        }
    | SearchTagsOnly


search : Plugins -> Config -> SearchConfig -> Model -> Html Msg
search plugins vc sc model =
    Html.Styled.form
        [ Css.form vc |> css
        , stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True ))
        , onSubmit UserHitsEnter
        ]
        [ div
            [ Css.frame vc |> css
            ]
            [ input
                ([ sc.css model.input |> css
                 , autocomplete False
                 , spellcheck False
                 , Locale.string vc.locale "The search" |> title
                 , onInput UserInputsSearch
                 , onEnter UserHitsEnter
                 , onFocus UserFocusSearch
                 , value model.input
                 ]
                    ++ (case sc.searchable of
                            SearchAll _ ->
                                [ "Addresses", "transaction", "label", "block", "actors" ]
                                    |> List.map (Locale.string vc.locale)
                                    |> (\st -> st ++ Plugin.searchPlaceholder plugins vc)
                                    |> String.join ", "
                                    |> placeholder
                                    |> List.singleton

                            SearchTagsOnly ->
                                [ Locale.string vc.locale "Label"
                                    |> placeholder
                                ]
                       )
                )
                []
            , searchResult plugins vc sc model
            ]
        , if sc.showIcon then
            button
                [ [ Css.Button.button vc |> Css.batch
                  , Css.Button.primary vc |> Css.batch
                  , Css.button vc |> Css.batch
                  ]
                    |> css
                , type_ "submit"
                ]
                [ FontAwesome.icon FontAwesome.search
                    |> Html.Styled.fromUnstyled
                ]

          else
            Util.View.none
        ]


searchResult : Plugins -> Config -> SearchConfig -> Model -> Html Msg
searchResult plugins vc sc model =
    resultList plugins vc sc model
        |> Autocomplete.dropdown vc
            { loading = model.loading
            , visible = model.visible
            , onClick = UserClicksResult
            }


filterByPrefix : String -> Api.Data.SearchResult -> Api.Data.SearchResult
filterByPrefix input result =
    { result
        | currencies =
            List.map
                (\currency ->
                    let
                        addr =
                            if String.toLower currency.currency == "eth" then
                                String.toLower input

                            else
                                input
                    in
                    { currency
                        | addresses = List.filter (String.startsWith addr) currency.addresses
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

        actorBadge =
            { title = Locale.string vc.locale "Actors"
            , badge =
                Maybe.map (.actors >> Maybe.withDefault []) found
                    |> Maybe.withDefault []
                    |> List.map (\x -> Actor ( x.id, x.label ))
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
                ++ [ actorBadge
                   , labelBadge
                   ]
                |> List.filterMap badgeToResult
            )
                ++ Plugin.searchResultList plugins pluginStates vc


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
                    ( Route.txRoute { currency = currency, txHash = a, table = Nothing, tokenTxId = Nothing }
                    , FontAwesome.exchangeAlt
                    , Util.View.truncate 50 a
                    )

                Block a ->
                    ( Route.blockRoute { currency = currency, block = a, table = Nothing }, FontAwesome.cube, String.fromInt a )

                Label a ->
                    ( Route.labelRoute a, FontAwesome.tag, a )

                Actor ( id, lbl ) ->
                    ( Route.actorRoute id Nothing, FontAwesome.user, lbl )

        el attr =
            if asLink then
                a ((Route.graphRoute route |> toUrl |> href) :: attr)

            else
                a (href "#" :: attr)
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
        (if String.length input < minSearchInputLength then
            []

         else
            Maybe.map
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


onEnter : msg -> Attribute msg
onEnter onEnterAction =
    preventDefaultOn "keypress" <|
        Json.Decode.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.Decode.succeed ( onEnterAction, True )

                else
                    Json.Decode.fail (String.fromInt keyCode)
            )
            keyCode
