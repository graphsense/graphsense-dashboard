module Update.Search exposing (batch, clear, getFirstResultUrl, popInput, update)

import Api.Data
import Bounce
import Effect exposing (n)
import Effect.Search as Effect exposing (Effect(..))
import Maybe.Extra
import Model.Search exposing (..)
import Msg.Search exposing (Msg(..))
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Result.Extra as RE
import Route exposing (toUrl)
import Route.Graph as Route


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        NoOp ->
            n model

        BrowserGotSearchResult result ->
            if model.loading then
                n
                    { model
                        | loading = False
                        , found = Just result
                    }

            else
                n model

        UserClicksResult ->
            -- handled upstream
            hide model |> n

        UserPicksCurrency _ ->
            -- handled upstream
            n model

        UserClicksResultLine _ ->
            n model

        UserInputsSearch input ->
            ( { model
                | input = input
                , bounce = Bounce.push model.bounce
              }
            , BounceEffect 200 RuntimeBounced
                :: (if model.loading then
                        [ CancelEffect ]

                    else
                        []
                   )
            )

        UserHitsEnter ->
            -- handled upstream
            n model

        UserFocusSearch ->
            n { model | visible = True }

        UserLeavesSearch ->
            hide model |> n

        BouncedBlur ->
            hide model |> n

        RuntimeBounced ->
            { model
                | bounce = Bounce.pop model.bounce
            }
                |> n
                |> maybeTriggerSearch

        PluginMsg msgValue ->
            -- handled in src/Update.elm
            n model


maybeTriggerSearch : ( Model, List Effect ) -> ( Model, List Effect )
maybeTriggerSearch ( model, cmd ) =
    let
        limit =
            10

        multi =
            getMulti model
    in
    if
        Bounce.steady model.bounce
            && (String.length model.input >= minSearchInputLength)
            && (List.length multi == 1)
    then
        ( { model
            | loading = True
          }
        , SearchEffect
            { query = model.input
            , currency = Nothing
            , limit = Just limit
            , toMsg = BrowserGotSearchResult
            }
            :: cmd
        )

    else
        model
            |> s_loading False
            |> n


getFirstResultUrl : Model -> Maybe String
getFirstResultUrl { input, found } =
    found
        |> Maybe.andThen
            (\{ currencies, labels } ->
                currencies
                    |> List.filter
                        (\{ addresses, txs } ->
                            List.isEmpty addresses
                                && List.isEmpty txs
                                |> not
                        )
                    |> List.head
                    |> Maybe.andThen
                        (\{ addresses, currency, txs } ->
                            addresses
                                |> List.head
                                |> Maybe.map
                                    (\address ->
                                        Route.addressRoute
                                            { currency = currency
                                            , address = address
                                            , table = Nothing
                                            , layer = Nothing
                                            }
                                    )
                                |> Maybe.Extra.orElse
                                    (txs
                                        |> List.head
                                        |> Maybe.map
                                            (\tx ->
                                                Route.txRoute
                                                    { currency = currency
                                                    , txHash = tx
                                                    , table = Nothing
                                                    , tokenTxId = Nothing
                                                    }
                                            )
                                    )
                        )
                    |> Maybe.Extra.orElse
                        (labels
                            |> List.head
                            |> Maybe.map Route.labelRoute
                        )
            )
        |> Maybe.Extra.orElse
            (if String.length input < 26 then
                Nothing

             else
                Route.addressRoute
                    { currency = "btc"
                    , address = input
                    , table = Nothing
                    , layer = Nothing
                    }
                    |> Just
            )
        |> Maybe.map (Route.graphRoute >> toUrl)


clear : Model -> Model
clear model =
    { model
        | found = Nothing
        , input = ""
        , loading = False
        , visible = False
    }


batch : String -> Model -> Model
batch currency model =
    { model | batch = Just ( currency, getMulti model ) }


popInput : Model -> ( Maybe ( String, String ), Model )
popInput model =
    model.batch
        |> Maybe.map
            (\( currency, terms ) ->
                case terms of
                    [] ->
                        ( Nothing, { model | batch = Nothing } )

                    term :: rest ->
                        ( Just ( currency, term ), { model | batch = Just ( currency, rest ) } )
            )
        |> Maybe.withDefault ( Nothing, model )


hide : Model -> Model
hide model =
    { model | visible = False }
