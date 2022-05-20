module Update.Graph.Browser exposing (..)

import Api.Data
import Effect exposing (n)
import Effect.Graph exposing (Effect(..))
import Init.Graph.Browser exposing (..)
import Init.Graph.Table as Table
import Model.Graph.Address as Address
import Model.Graph.Browser exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Table exposing (..)
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Route.Graph as Route
import Table
import Tuple exposing (..)
import Update.Graph.Table exposing (appendData)


loadingAddress : { currency : String, address : String } -> Model -> Model
loadingAddress id model =
    { model
        | type_ = Address (Loading id.currency id.address) Nothing
        , visible = True
    }


loadingEntity : { currency : String, entity : Int } -> Model -> Model
loadingEntity id model =
    { model
        | type_ = Entity (Loading id.currency id.entity) Nothing
        , visible = True
    }


showAddressTable : Route.AddressTable -> Model -> ( Model, List Effect )
showAddressTable route model =
    let
        ( type_, effects ) =
            case model.type_ of
                Address loadable t ->
                    let
                        ( currency, address ) =
                            case loadable of
                                Loading curr addr ->
                                    ( curr, addr )

                                Loaded a ->
                                    ( a.address.currency, a.address.address )
                    in
                    mapFirst (Address loadable) <|
                        case ( route, t ) of
                            ( Route.AddressTagsTable, Just (AddressTagsTable _) ) ->
                                n t

                            ( Route.AddressTagsTable, _ ) ->
                                ( AddressTagsTable Table.init |> Just
                                , []
                                )

                            ( Route.AddressTxsTable, Just (AddressTxsTable _) ) ->
                                n t

                            ( Route.AddressTxsTable, _ ) ->
                                ( AddressTxsTable Table.init |> Just
                                , [ GetAddressTxsEffect
                                        { currency = currency
                                        , address = address
                                        , nextpage = Nothing
                                        , pagesize = 100
                                        , toMsg = BrowserGotAddressTxs { currency = currency, address = address }
                                        }
                                  ]
                                )

                            ( Route.AddressIncomingNeighborsTable, Just (AddressIncomingNeighborsTable _) ) ->
                                n t

                            ( Route.AddressIncomingNeighborsTable, _ ) ->
                                ( AddressIncomingNeighborsTable Table.init |> Just
                                , []
                                )

                            ( Route.AddressOutgoingNeighborsTable, Just (AddressOutgoingNeighborsTable _) ) ->
                                n t

                            ( Route.AddressOutgoingNeighborsTable, _ ) ->
                                ( AddressOutgoingNeighborsTable Table.init |> Just
                                , []
                                )

                _ ->
                    ( model.type_, [] )
    in
    ( { model
        | type_ = type_
      }
    , effects
    )


showEntityTable : Route.EntityTable -> Model -> ( Model, List Effect )
showEntityTable route model =
    ( model, [] )


show : Model -> Model
show model =
    { model
        | visible = True
    }


showEntity : Entity.Entity -> Model -> Model
showEntity entity model =
    show model
        |> s_type_ (Entity (Loaded entity) Nothing)


showAddress : Address.Address -> Model -> Model
showAddress address model =
    show model
        |> s_type_
            (Address (Loaded address) <|
                case model.type_ of
                    Address loadable table ->
                        if
                            loadableAddressId loadable
                                == address.address.address
                                && loadableAddressCurrency loadable
                                == address.address.currency
                        then
                            table

                        else
                            Nothing

                    _ ->
                        Nothing
            )


showAddressTxs : { currency : String, address : String } -> Api.Data.AddressTxs -> Model -> Model
showAddressTxs id data model =
    let
        addressTxs =
            data.addressTxs
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.AddressTxAddressTxUtxo tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Address loadable table ->
            if matchAddressId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Address loadable <|
                            case table of
                                Just (AddressTxsTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddressTxsTable
                                        |> Just

                                _ ->
                                    Table.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsTable
                                        |> Just
                }

        _ ->
            model


matchAddressId : { currency : String, address : String } -> Loadable String Address.Address -> Bool
matchAddressId { currency, address } loadable =
    case loadable of
        Loading c id ->
            c == currency && id == address

        Loaded a ->
            a.address.currency == currency && a.address.address == address


tableNewState : Table.State -> Model -> Model
tableNewState state model =
    { model
        | type_ =
            case model.type_ of
                Address loadable table ->
                    Address loadable <|
                        case table of
                            Just (AddressTxsTable t) ->
                                { t | state = state }
                                    |> AddressTxsTable
                                    |> Just

                            _ ->
                                table

                _ ->
                    model.type_
    }
