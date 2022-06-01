module Update.Graph.Browser exposing (..)

import Api.Data
import Effect exposing (n)
import Effect.Graph exposing (Effect(..))
import Init.Graph.Browser exposing (..)
import Init.Graph.Table as Table
import Json.Encode
import Log
import Model.Graph.Address as Address
import Model.Graph.Browser exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Model.Graph.Table exposing (..)
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Route.Graph as Route
import Table
import Tuple exposing (..)
import Update.Graph.Table exposing (appendData)
import View.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import View.Graph.Table.AddressTagsTable as AddressTagsTable
import View.Graph.Table.AddressTxsTable as AddressTxsTable
import View.Graph.Table.EntityAddressesTable as EntityAddressesTable
import View.Graph.Table.EntityNeighborsTable as EntityNeighborsTable


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
            createAddressTable route t currency address
                |> mapFirst (Address loadable)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createAddressTable : Route.AddressTable -> Maybe AddressTable -> String -> String -> ( Maybe AddressTable, List Effect )
createAddressTable route t currency address =
    case ( route, t ) of
        ( Route.AddressTagsTable, Just (AddressTagsTable _) ) ->
            n t

        ( Route.AddressTagsTable, _ ) ->
            ( AddressTagsTable.init |> AddressTagsTable |> Just
            , [ GetAddressTagsEffect
                    { currency = currency
                    , address = address
                    , pagesize = 100
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotAddressTagsTable
                            { currency = currency
                            , address = address
                            }
                    }
              ]
            )

        ( Route.AddressTxsTable, Just (AddressTxsTable _) ) ->
            n t

        ( Route.AddressTxsTable, _ ) ->
            ( AddressTxsTable.init |> AddressTxsTable |> Just
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
            ( AddressNeighborsTable.init False |> AddressIncomingNeighborsTable |> Just
            , [ GetAddressNeighborsEffect
                    { currency = currency
                    , address = address
                    , isOutgoing = False
                    , pagesize = 100
                    , includeLabels = True
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotAddressNeighborsTable
                            { currency = currency
                            , address = address
                            }
                            False
                    }
              ]
            )

        ( Route.AddressOutgoingNeighborsTable, Just (AddressOutgoingNeighborsTable _) ) ->
            n t

        ( Route.AddressOutgoingNeighborsTable, _ ) ->
            ( AddressNeighborsTable.init True |> AddressOutgoingNeighborsTable |> Just
            , [ GetAddressNeighborsEffect
                    { currency = currency
                    , address = address
                    , isOutgoing = True
                    , pagesize = 100
                    , includeLabels = True
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotAddressNeighborsTable
                            { currency = currency
                            , address = address
                            }
                            True
                    }
              ]
            )


showEntityTable : Route.EntityTable -> Model -> ( Model, List Effect )
showEntityTable route model =
    case model.type_ |> Log.log "showEntityTable" of
        Entity loadable t ->
            let
                ( currency, entity ) =
                    case loadable of
                        Loading curr e ->
                            ( curr, e )

                        Loaded a ->
                            ( a.entity.currency, a.entity.entity )
            in
            createEntityTable route t currency entity
                |> Log.log "table"
                |> mapFirst (Entity loadable)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createEntityTable : Route.EntityTable -> Maybe EntityTable -> String -> Int -> ( Maybe EntityTable, List Effect )
createEntityTable route t currency entity =
    case ( route, t ) of
        ( Route.EntityTagsTable, Just (EntityTagsTable _) ) ->
            n t

        ( Route.EntityTagsTable, _ ) ->
            ( AddressTagsTable.init |> EntityTagsTable |> Just
            , [ GetEntityAddressTagsEffect
                    { currency = currency
                    , entity = entity
                    , pagesize = 100
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotEntityAddressTagsTable
                            { currency = currency
                            , entity = entity
                            }
                    }
              ]
            )

        ( Route.EntityTxsTable, Just (EntityTxsTable _) ) ->
            n t

        ( Route.EntityTxsTable, _ ) ->
            ( AddressTxsTable.init |> EntityTxsTable |> Just
            , [ GetEntityTxsEffect
                    { currency = currency
                    , entity = entity
                    , nextpage = Nothing
                    , pagesize = 100
                    , toMsg = BrowserGotEntityTxs { currency = currency, entity = entity }
                    }
              ]
            )

        ( Route.EntityIncomingNeighborsTable, Just (EntityIncomingNeighborsTable _) ) ->
            n t

        ( Route.EntityIncomingNeighborsTable, _ ) ->
            ( EntityNeighborsTable.init False |> EntityIncomingNeighborsTable |> Just
            , [ GetEntityNeighborsEffect
                    { currency = currency
                    , entity = entity
                    , isOutgoing = False
                    , onlyIds = Nothing
                    , pagesize = 100
                    , includeLabels = True
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotEntityNeighborsTable
                            { currency = currency
                            , entity = entity
                            }
                            False
                    }
              ]
            )

        ( Route.EntityOutgoingNeighborsTable, Just (EntityOutgoingNeighborsTable _) ) ->
            n t

        ( Route.EntityOutgoingNeighborsTable, _ ) ->
            ( EntityNeighborsTable.init False |> EntityOutgoingNeighborsTable |> Just
            , [ GetEntityNeighborsEffect
                    { currency = currency
                    , entity = entity
                    , isOutgoing = True
                    , onlyIds = Nothing
                    , pagesize = 100
                    , includeLabels = True
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotEntityNeighborsTable
                            { currency = currency
                            , entity = entity
                            }
                            True
                    }
              ]
            )

        ( Route.EntityAddressesTable, Just (EntityAddressesTable _) ) ->
            n t

        ( Route.EntityAddressesTable, _ ) ->
            ( EntityAddressesTable.init |> EntityAddressesTable |> Just
            , [ GetEntityAddressesEffect
                    { currency = currency
                    , entity = entity
                    , nextpage = Nothing
                    , pagesize = 100
                    , toMsg = BrowserGotEntityAddressesForTable { currency = currency, entity = entity }
                    }
              ]
            )


show : Model -> Model
show model =
    { model
        | visible = True
    }


showEntity : Entity.Entity -> Model -> Model
showEntity entity model =
    show model
        |> s_type_
            (Entity (Loaded entity) <|
                case model.type_ of
                    Entity loadable table ->
                        if
                            loadableEntityId loadable
                                == entity.entity.entity
                                && loadableEntityCurrency loadable
                                == entity.entity.currency
                        then
                            table

                        else
                            Nothing

                    _ ->
                        Nothing
            )


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
                                    AddressTxsTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsTable
                                        |> Just
                }

        _ ->
            model


showAddressTags : { currency : String, address : String } -> Api.Data.AddressTags -> Model -> Model
showAddressTags id data model =
    case model.type_ of
        Address loadable table ->
            if matchAddressId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Address loadable <|
                            case table of
                                Just (AddressTagsTable t) ->
                                    appendData data.nextPage data.addressTags t
                                        |> AddressTagsTable
                                        |> Just

                                _ ->
                                    AddressTagsTable.init
                                        |> s_data data.addressTags
                                        |> s_nextpage data.nextPage
                                        |> AddressTagsTable
                                        |> Just
                }

        _ ->
            model


showAddressNeighbors : { currency : String, address : String } -> Bool -> Api.Data.NeighborAddresses -> Model -> Model
showAddressNeighbors id isOutgoing data model =
    case model.type_ of
        Address loadable table ->
            if matchAddressId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Address loadable <|
                            case ( isOutgoing, table ) of
                                ( True, Just (AddressOutgoingNeighborsTable t) ) ->
                                    appendData data.nextPage data.neighbors t
                                        |> AddressOutgoingNeighborsTable
                                        |> Just

                                ( False, Just (AddressIncomingNeighborsTable t) ) ->
                                    appendData data.nextPage data.neighbors t
                                        |> AddressIncomingNeighborsTable
                                        |> Just

                                _ ->
                                    AddressNeighborsTable.init isOutgoing
                                        |> s_data data.neighbors
                                        |> s_nextpage data.nextPage
                                        |> (if isOutgoing then
                                                AddressOutgoingNeighborsTable

                                            else
                                                AddressIncomingNeighborsTable
                                           )
                                        |> Just
                }

        _ ->
            model


showEntityNeighbors : { currency : String, entity : Int } -> Bool -> Api.Data.NeighborEntities -> Model -> Model
showEntityNeighbors id isOutgoing data model =
    case model.type_ of
        Entity loadable table ->
            if matchEntityId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Entity loadable <|
                            case ( isOutgoing, table ) of
                                ( True, Just (EntityOutgoingNeighborsTable t) ) ->
                                    appendData data.nextPage data.neighbors t
                                        |> EntityOutgoingNeighborsTable
                                        |> Just

                                ( False, Just (EntityIncomingNeighborsTable t) ) ->
                                    appendData data.nextPage data.neighbors t
                                        |> EntityIncomingNeighborsTable
                                        |> Just

                                _ ->
                                    EntityNeighborsTable.init isOutgoing
                                        |> s_data data.neighbors
                                        |> s_nextpage data.nextPage
                                        |> (if isOutgoing then
                                                EntityOutgoingNeighborsTable

                                            else
                                                EntityIncomingNeighborsTable
                                           )
                                        |> Just
                }

        _ ->
            model


showEntityAddresses : { currency : String, entity : Int } -> Api.Data.EntityAddresses -> Model -> Model
showEntityAddresses id data model =
    case model.type_ of
        Entity loadable table ->
            if matchEntityId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Entity loadable <|
                            case table of
                                Just (EntityAddressesTable t) ->
                                    appendData data.nextPage data.addresses t
                                        |> EntityAddressesTable
                                        |> Just

                                _ ->
                                    EntityAddressesTable.init
                                        |> s_data data.addresses
                                        |> s_nextpage data.nextPage
                                        |> EntityAddressesTable
                                        |> Just
                }

        _ ->
            model


showEntityTxs : { currency : String, entity : Int } -> Api.Data.AddressTxs -> Model -> Model
showEntityTxs id data model =
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
        Entity loadable table ->
            if matchEntityId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Entity loadable <|
                            case table of
                                Just (EntityTxsTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> EntityTxsTable
                                        |> Just

                                _ ->
                                    AddressTxsTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> EntityTxsTable
                                        |> Just
                }

        _ ->
            model


showEntityAddressTags : { currency : String, entity : Int } -> Api.Data.AddressTags -> Model -> Model
showEntityAddressTags id data model =
    case model.type_ of
        Entity loadable table ->
            if matchEntityId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        Entity loadable <|
                            case table of
                                Just (EntityTagsTable t) ->
                                    appendData data.nextPage data.addressTags t
                                        |> EntityTagsTable
                                        |> Just

                                _ ->
                                    AddressTagsTable.init
                                        |> s_data data.addressTags
                                        |> s_nextpage data.nextPage
                                        |> EntityTagsTable
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


matchEntityId : { currency : String, entity : Int } -> Loadable Int Entity.Entity -> Bool
matchEntityId { currency, entity } loadable =
    case loadable of
        Loading c id ->
            c == currency && id == entity

        Loaded a ->
            a.entity.currency == currency && a.entity.entity == entity


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

                            Just (AddressTagsTable t) ->
                                { t | state = state }
                                    |> AddressTagsTable
                                    |> Just

                            Just (AddressIncomingNeighborsTable t) ->
                                { t | state = state }
                                    |> AddressIncomingNeighborsTable
                                    |> Just

                            Just (AddressOutgoingNeighborsTable t) ->
                                { t | state = state }
                                    |> AddressOutgoingNeighborsTable
                                    |> Just

                            Nothing ->
                                table

                Entity loadable table ->
                    Entity loadable <|
                        case table of
                            Just (EntityAddressesTable t) ->
                                { t | state = state }
                                    |> EntityAddressesTable
                                    |> Just

                            Just (EntityTxsTable t) ->
                                { t | state = state }
                                    |> EntityTxsTable
                                    |> Just

                            Just (EntityTagsTable t) ->
                                { t | state = state }
                                    |> EntityTagsTable
                                    |> Just

                            Just (EntityIncomingNeighborsTable t) ->
                                { t | state = state }
                                    |> EntityIncomingNeighborsTable
                                    |> Just

                            Just (EntityOutgoingNeighborsTable t) ->
                                { t | state = state }
                                    |> EntityOutgoingNeighborsTable
                                    |> Just

                            Nothing ->
                                table

                _ ->
                    model.type_
    }


showPlugin : String -> Model -> Model
showPlugin pid model =
    show model
        |> s_type_ (Plugin pid)


setHeight : Float -> Model -> Model
setHeight height browser =
    { browser
        | height = Just height
    }
