module Update.Search exposing (clear, resultLineToRoute, update)

import Api.Data
import Autocomplete
import Bounce
import Effect exposing (n)
import Effect.Search as Effect exposing (Effect(..))
import Init.Search exposing (init)
import Maybe.Extra
import Model.Search exposing (..)
import Msg.Search exposing (Msg(..))
import Process
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Route exposing (toUrl)
import Route.Graph as Graph
import Task
import Tuple exposing (pair)


currencyToResult : String -> Api.Data.SearchResult -> ( String, Int ) -> List ResultLine
currencyToResult query found ( currency, latestBlock ) =
    (found.currencies
        |> List.filter (.currency >> (==) currency)
        |> List.head
        |> Maybe.map
            (\{ addresses, txs } ->
                List.map (Address currency) addresses
                    ++ List.map (Tx currency) txs
            )
        |> Maybe.withDefault []
    )
        ++ blocksToResult query currency latestBlock


blocksToResult : String -> String -> Int -> List ResultLine
blocksToResult input currency latestBlock =
    String.toInt input
        |> Maybe.map
            (\i ->
                if i >= 0 && i <= latestBlock then
                    [ Block currency i ]

                else
                    []
            )
        |> Maybe.withDefault []


searchResultToResultLines : String -> List ( String, Int ) -> Api.Data.SearchResult -> List ResultLine
searchResultToResultLines query latestBlocks searchResult =
    latestBlocks
        |> List.map (currencyToResult query searchResult)
        |> List.concat


labelResultLines : Api.Data.SearchResult -> List ResultLine
labelResultLines =
    .labels
        >> List.map Label


actorResultLines : Api.Data.SearchResult -> List ResultLine
actorResultLines =
    .actors
        >> Maybe.withDefault []
        >> List.map (\x -> Actor ( x.id, x.label ))


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
                        , txs = List.filter (\x -> String.startsWith (removeLeading0x input) (removeLeading0x x)) currency.txs
                    }
                )
                result.currencies
    }


removeLeading0x : String -> String
removeLeading0x s =
    if String.startsWith "0x" s then
        s |> String.dropLeft 2

    else
        s


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        NoOp ->
            n model

        BrowserGotSearchResult query res ->
            let
                result =
                    filterByPrefix query res

                choices =
                    { choices =
                        case model.searchType of
                            SearchAll { latestBlocks } ->
                                searchResultToResultLines query latestBlocks result
                                    ++ actorResultLines result
                                    ++ labelResultLines result

                            SearchTagsOnly ->
                                labelResultLines result
                    , query = query
                    , ignoreList = []
                    }
            in
            { model
                | autocomplete =
                    Autocomplete.onFetch (Ok choices) model.autocomplete
                        |> Autocomplete.setSelectedIndex 0
            }
                |> n

        UserFocusSearch ->
            n
                { model
                    | visible = True
                }

        UserLeavesSearch ->
            case model.searchType of
                SearchAll { pickingCurrency } ->
                    if pickingCurrency then
                        n model

                    else
                        hide model
                            |> n

                SearchTagsOnly ->
                    hide model
                        |> n

        UserPicksCurrency _ ->
            -- handled upstream
            n
                { model
                    | searchType =
                        case model.searchType of
                            SearchAll sa ->
                                { sa | pickingCurrency = False }
                                    |> SearchAll

                            SearchTagsOnly ->
                                SearchTagsOnly
                }

        UserClickedCloseCurrencyPicker ->
            -- handled upstream
            n model

        UserClicksResultLine ->
            clear model
                |> n

        PluginMsg msgValue ->
            -- handled in src/Update.elm
            n model

        AutocompleteMsg ms ->
            let
                ( ac, doFetch, cmd ) =
                    Autocomplete.update ms model.autocomplete

                m2 =
                    { model
                        | autocomplete = ac
                        , visible =
                            Autocomplete.query ac
                                |> String.isEmpty
                                |> not
                    }
            in
            CmdEffect (Cmd.map AutocompleteMsg cmd)
                :: (if Debug.log "doFetch" doFetch then
                        maybeTriggerSearch m2

                    else
                        []
                   )
                |> pair m2


maybeTriggerSearch : Model -> List Effect
maybeTriggerSearch model =
    let
        limit =
            10

        query =
            Autocomplete.query model.autocomplete

        isPickingCurrency =
            Debug.log "isPickingCurrency" <|
                case model.searchType of
                    SearchAll { pickingCurrency } ->
                        pickingCurrency

                    SearchTagsOnly ->
                        False
    in
    if not isPickingCurrency && not (isLikelyPathSearchInput query) |> Debug.log "isliek" then
        SearchEffect
            { query = query
            , currency = Nothing
            , limit = Just limit
            , toMsg = BrowserGotSearchResult query
            }
            |> List.singleton

    else
        []


resultLineToRoute : ResultLine -> Graph.Route
resultLineToRoute resultLine =
    case resultLine of
        Address currency address ->
            Graph.addressRoute
                { currency = currency
                , address = address
                , table = Nothing
                , layer = Nothing
                }

        Tx currency tx ->
            Graph.txRoute
                { currency = currency
                , txHash = tx
                , table = Nothing
                , tokenTxId = Nothing
                }

        Block currency block ->
            Graph.blockRoute
                { currency = currency
                , block = block
                , table = Nothing
                }

        Label label ->
            Graph.labelRoute label

        Actor ( id, label ) ->
            Graph.actorRoute id Nothing


clear : Model -> Model
clear model =
    init model.searchType


hide : Model -> Model
hide model =
    { model | visible = False }
        |> setQuery (query model)
