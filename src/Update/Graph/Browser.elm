module Update.Graph.Browser exposing (..)

import Api.Data
import Effect exposing (n)
import Effect.Api exposing (Effect(..))
import Effect.Graph exposing (Effect(..))
import Init.Graph.Browser exposing (..)
import Init.Graph.Table as Table
import Init.Graph.Tag as Tag
import Json.Encode
import Log
import Model.Address as A
import Model.Block as B
import Model.Entity as E
import Model.Graph.Address as Address
import Model.Graph.Browser exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Model.Graph.Link exposing (Link)
import Model.Graph.Table exposing (..)
import Model.Graph.Tag as Tag
import Model.Search as Search
import Msg.Graph exposing (Msg(..))
import Msg.Search as Search
import RecordSetter exposing (..)
import Route.Graph as Route
import Table
import Tuple exposing (..)
import Update.Graph.Table exposing (appendData, applyFilter, setData)
import Update.Search as Search
import View.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import View.Graph.Table.AddressTagsTable as AddressTagsTable
import View.Graph.Table.AddressTxsUtxoTable as AddressTxsUtxoTable
import View.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import View.Graph.Table.EntityAddressesTable as EntityAddressesTable
import View.Graph.Table.EntityNeighborsTable as EntityNeighborsTable
import View.Graph.Table.LabelAddressTagsTable as LabelAddressTagsTable
import View.Graph.Table.TxUtxoTable as TxUtxoTable
import View.Graph.Table.TxsAccountTable as TxsAccountTable
import View.Graph.Table.TxsUtxoTable as TxsUtxoTable
import View.Graph.Table.UserAddressTagsTable as UserAddressTagsTable


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


loadingBlock : { currency : String, block : Int } -> Model -> Model
loadingBlock id model =
    { model
        | type_ = Block (Loading id.currency id.block) Nothing
        , visible = True
    }


loadingTxAccount : { currency : String, txHash : String } -> Model -> ( Model, List Effect )
loadingTxAccount id model =
    let
        ( type_, eff ) =
            ( TxAccount (Loading id.currency id.txHash)
            , [ GetTxEffect
                    { txHash = id.txHash
                    , currency = id.currency
                    }
                    BrowserGotTx
                    |> ApiEffect
              ]
            )
    in
    ( { model
        | type_ = type_
        , visible = True
      }
    , eff
    )


loadingTxUtxo : { currency : String, txHash : String } -> Model -> ( Model, List Effect )
loadingTxUtxo id model =
    let
        ( type_, eff ) =
            ( TxUtxo (Loading id.currency id.txHash) Nothing
            , [ GetTxEffect
                    { txHash = id.txHash
                    , currency = id.currency
                    }
                    BrowserGotTx
                    |> ApiEffect
              ]
            )
    in
    ( { model
        | type_ = type_
        , visible = True
      }
    , eff
    )


loadingLabel : String -> Model -> ( Model, List Effect )
loadingLabel label model =
    ( { model
        | type_ =
            LabelAddressTagsTable.init
                |> Label label
        , visible = True
      }
    , [ listAddressTagsEffect label Nothing
      ]
    )


showAddresslink : { source : Address.Address, link : Link Address.Address } -> Model -> Model
showAddresslink { source, link } model =
    { model
        | type_ = Addresslink source link Nothing
        , visible = True
    }


showEntitylink : { source : Entity.Entity, link : Link Entity.Entity } -> Model -> Model
showEntitylink { source, link } model =
    { model
        | type_ = Entitylink source link Nothing
        , visible = True
    }


showUserTags : List Tag.UserTag -> Model -> Model
showUserTags tags model =
    { model
        | type_ =
            UserAddressTagsTable.init
                |> appendData Nothing tags
                |> UserTags
        , visible = True
    }


showLabelAddressTags : String -> Api.Data.AddressTags -> Model -> Model
showLabelAddressTags label data model =
    case model.type_ of
        Label current table ->
            if current /= label then
                model

            else
                { model
                    | type_ =
                        Label current <|
                            appendData data.nextPage data.addressTags table
                }

        _ ->
            model


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
            , [ getAddressTagsEffect
                    { currency = currency
                    , address = address
                    }
                    Nothing
              ]
            )

        ( Route.AddressTxsTable, Just (AddressTxsUtxoTable _) ) ->
            n t

        ( Route.AddressTxsTable, Just (AddressTxsAccountTable _) ) ->
            n t

        ( Route.AddressTxsTable, _ ) ->
            if String.toLower currency == "eth" then
                ( TxsAccountTable.init |> AddressTxsAccountTable |> Just
                , [ getAddressTxsEffect
                        { currency = currency
                        , address = address
                        }
                        Nothing
                  ]
                )

            else
                ( AddressTxsUtxoTable.init |> AddressTxsUtxoTable |> Just
                , [ getAddressTxsEffect
                        { currency = currency
                        , address = address
                        }
                        Nothing
                  ]
                )

        ( Route.AddressIncomingNeighborsTable, Just (AddressIncomingNeighborsTable _) ) ->
            n t

        ( Route.AddressIncomingNeighborsTable, _ ) ->
            ( AddressNeighborsTable.init False |> AddressIncomingNeighborsTable |> Just
            , [ getAddressNeighborsEffect
                    False
                    { currency = currency
                    , address = address
                    }
                    Nothing
              ]
            )

        ( Route.AddressOutgoingNeighborsTable, Just (AddressOutgoingNeighborsTable _) ) ->
            n t

        ( Route.AddressOutgoingNeighborsTable, _ ) ->
            ( AddressNeighborsTable.init True |> AddressOutgoingNeighborsTable |> Just
            , [ getAddressNeighborsEffect
                    True
                    { currency = currency
                    , address = address
                    }
                    Nothing
              ]
            )


showAddresslinkTable : Route.AddresslinkTable -> Model -> ( Model, List Effect )
showAddresslinkTable route model =
    case model.type_ of
        Addresslink source link t ->
            let
                currency =
                    Id.currency source.id
            in
            createAddresslinkTable route t currency (Id.addressId source.id) (Id.addressId link.node.id)
                |> mapFirst (Addresslink source link)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


showEntitylinkTable : Route.AddresslinkTable -> Model -> ( Model, List Effect )
showEntitylinkTable route model =
    case model.type_ of
        Entitylink source link t ->
            let
                currency =
                    Id.currency source.id
            in
            createEntitylinkTable route t currency (Id.entityId source.id) (Id.entityId link.node.id)
                |> mapFirst (Entitylink source link)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createAddresslinkTable : Route.AddresslinkTable -> Maybe AddresslinkTable -> String -> String -> String -> ( Maybe AddresslinkTable, List Effect )
createAddresslinkTable route t currency source target =
    case ( route, t ) of
        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsUtxoTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsAccountTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, Nothing ) ->
            if String.toLower currency == "eth" then
                ( TxsAccountTable.init |> AddresslinkTxsAccountTable |> Just
                , [ getAddresslinkTxsEffect
                        { currency = currency
                        , source = source
                        , target = target
                        }
                        Nothing
                  ]
                )

            else
                ( AddresslinkTxsUtxoTable.init |> AddresslinkTxsUtxoTable |> Just
                , [ getAddresslinkTxsEffect
                        { currency = currency
                        , source = source
                        , target = target
                        }
                        Nothing
                  ]
                )


createEntitylinkTable : Route.AddresslinkTable -> Maybe AddresslinkTable -> String -> Int -> Int -> ( Maybe AddresslinkTable, List Effect )
createEntitylinkTable route t currency source target =
    case ( route, t ) of
        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsUtxoTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsAccountTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, Nothing ) ->
            if String.toLower currency == "eth" then
                ( TxsAccountTable.init |> AddresslinkTxsAccountTable |> Just
                , [ getEntitylinkTxsEffect
                        { currency = currency
                        , source = source
                        , target = target
                        }
                        Nothing
                  ]
                )

            else
                ( AddresslinkTxsUtxoTable.init |> AddresslinkTxsUtxoTable |> Just
                , [ getEntitylinkTxsEffect
                        { currency = currency
                        , source = source
                        , target = target
                        }
                        Nothing
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
            , [ getEntityAddressTagsEffect
                    { currency = currency
                    , entity = entity
                    }
                    Nothing
              ]
            )

        ( Route.EntityTxsTable, Just (EntityTxsUtxoTable _) ) ->
            n t

        ( Route.EntityTxsTable, Just (EntityTxsAccountTable _) ) ->
            n t

        ( Route.EntityTxsTable, _ ) ->
            if String.toLower currency == "eth" then
                ( TxsAccountTable.init |> EntityTxsAccountTable |> Just
                , [ getEntityTxsEffect
                        { currency = currency
                        , entity = entity
                        }
                        Nothing
                  ]
                )

            else
                ( AddressTxsUtxoTable.init |> EntityTxsUtxoTable |> Just
                , [ getEntityTxsEffect
                        { currency = currency
                        , entity = entity
                        }
                        Nothing
                  ]
                )

        ( Route.EntityIncomingNeighborsTable, Just (EntityIncomingNeighborsTable _) ) ->
            n t

        ( Route.EntityIncomingNeighborsTable, _ ) ->
            ( EntityNeighborsTable.init False |> EntityIncomingNeighborsTable |> Just
            , [ getEntityNeighborsEffect
                    False
                    { currency = currency
                    , entity = entity
                    }
                    Nothing
              ]
            )

        ( Route.EntityOutgoingNeighborsTable, Just (EntityOutgoingNeighborsTable _) ) ->
            n t

        ( Route.EntityOutgoingNeighborsTable, _ ) ->
            ( EntityNeighborsTable.init False |> EntityOutgoingNeighborsTable |> Just
            , [ getEntityNeighborsEffect
                    True
                    { currency = currency
                    , entity = entity
                    }
                    Nothing
              ]
            )

        ( Route.EntityAddressesTable, Just (EntityAddressesTable _) ) ->
            n t

        ( Route.EntityAddressesTable, _ ) ->
            ( EntityAddressesTable.init |> EntityAddressesTable |> Just
            , [ getEntityAddressesEffect
                    { currency = currency
                    , entity = entity
                    }
                    Nothing
              ]
            )


showBlockTable : Route.BlockTable -> Model -> ( Model, List Effect )
showBlockTable route model =
    case model.type_ |> Log.log "showBlockTable" of
        Block loadable t ->
            let
                ( currency, block ) =
                    case loadable of
                        Loading curr e ->
                            ( curr, e )

                        Loaded a ->
                            ( a.currency, a.height )
            in
            createBlockTable route t currency block
                |> Log.log "table"
                |> mapFirst (Block loadable)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createBlockTable : Route.BlockTable -> Maybe BlockTable -> String -> Int -> ( Maybe BlockTable, List Effect )
createBlockTable route t currency block =
    case ( route, t ) of
        ( Route.BlockTxsTable, Just (BlockTxsUtxoTable _) ) ->
            n t

        ( Route.BlockTxsTable, Just (BlockTxsAccountTable _) ) ->
            n t

        ( Route.BlockTxsTable, Nothing ) ->
            if String.toLower currency == "eth" then
                ( TxsAccountTable.init |> BlockTxsAccountTable |> Just
                , [ getBlockTxsEffect
                        { currency = currency
                        , block = block
                        }
                        Nothing
                  ]
                )

            else
                ( TxsUtxoTable.init |> BlockTxsUtxoTable |> Just
                , [ getBlockTxsEffect
                        { currency = currency
                        , block = block
                        }
                        Nothing
                  ]
                )


showTxUtxoTable : Route.TxTable -> Model -> ( Model, List Effect )
showTxUtxoTable route model =
    case model.type_ of
        TxUtxo loadable t ->
            let
                ( currency, txHash, tx ) =
                    case loadable of
                        Loading curr tx_ ->
                            ( curr, tx_, Nothing )

                        Loaded a ->
                            ( a.currency, a.txHash, Just a )
            in
            createTxUtxoTable route t currency txHash tx
                |> mapFirst (TxUtxo loadable)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createTxUtxoTable : Route.TxTable -> Maybe TxUtxoTable -> String -> String -> Maybe Api.Data.TxUtxo -> ( Maybe TxUtxoTable, List Effect )
createTxUtxoTable route t currency txHash tx =
    case ( route, t ) of
        ( Route.TxInputsTable, Just (TxUtxoInputsTable _) ) ->
            n t

        ( Route.TxInputsTable, _ ) ->
            case Maybe.andThen .inputs tx of
                Nothing ->
                    ( TxUtxoTable.init False
                        |> TxUtxoInputsTable
                        |> Just
                    , [ GetTxUtxoAddressesEffect
                            { currency = currency
                            , txHash = txHash
                            , isOutgoing = False
                            }
                            (BrowserGotTxUtxoAddresses { currency = currency, txHash = txHash } False)
                            |> ApiEffect
                      ]
                    )

                Just inputs ->
                    ( TxUtxoTable.init False
                        |> appendData Nothing inputs
                        |> TxUtxoInputsTable
                        |> Just
                    , []
                    )

        ( Route.TxOutputsTable, Just (TxUtxoOutputsTable _) ) ->
            n t

        ( Route.TxOutputsTable, _ ) ->
            case Maybe.andThen .outputs tx of
                Nothing ->
                    ( TxUtxoTable.init True
                        |> TxUtxoOutputsTable
                        |> Just
                    , [ GetTxUtxoAddressesEffect
                            { currency = currency
                            , txHash = txHash
                            , isOutgoing = True
                            }
                            (BrowserGotTxUtxoAddresses { currency = currency, txHash = txHash } True)
                            |> ApiEffect
                      ]
                    )

                Just outputs ->
                    ( TxUtxoTable.init True
                        |> appendData Nothing outputs
                        |> TxUtxoOutputsTable
                        |> Just
                    , []
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


showTx : Api.Data.Tx -> Model -> Model
showTx data model =
    show model
        |> s_type_
            (case data of
                Api.Data.TxTxUtxo tx ->
                    TxUtxo (Loaded tx) <|
                        case model.type_ of
                            TxUtxo loadable table ->
                                if
                                    loadableTxId loadable
                                        == tx.txHash
                                        && loadableCurrency loadable
                                        == tx.currency
                                then
                                    table

                                else
                                    Nothing

                            _ ->
                                Nothing

                Api.Data.TxTxAccount tx ->
                    TxAccount (Loaded tx)
            )


showBlock : Api.Data.Block -> Model -> Model
showBlock block model =
    show model
        |> s_type_
            (Block (Loaded block) <|
                case model.type_ of
                    Block loadable table ->
                        if
                            loadableBlockId loadable
                                == block.height
                                && loadableCurrency loadable
                                == block.currency
                        then
                            table

                        else
                            Nothing

                    _ ->
                        Nothing
            )


showBlockTxsUtxo : { currency : String, block : Int } -> List Api.Data.Tx -> Model -> Model
showBlockTxsUtxo id data model =
    let
        blockTxs =
            data
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.TxTxUtxo tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Block loadable table ->
            if matchBlockId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        TxsUtxoTable.init
                            |> appendData Nothing blockTxs
                            |> BlockTxsUtxoTable
                            |> Just
                            |> Block loadable
                }

        _ ->
            model


showBlockTxsAccount : { currency : String, block : Int } -> List Api.Data.Tx -> Model -> Model
showBlockTxsAccount id data model =
    let
        blockTxs =
            data
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.TxTxAccount tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Block loadable table ->
            if matchBlockId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        TxsAccountTable.init
                            |> appendData Nothing blockTxs
                            |> BlockTxsAccountTable
                            |> Just
                            |> Block loadable
                }

        _ ->
            model


updateAddress : A.Address -> (Address.Address -> Address.Address) -> Model -> Model
updateAddress { currency, address } update model =
    case model.type_ of
        Address (Loaded a) table ->
            if a.address.currency == currency && a.address.address == address then
                { model
                    | type_ = Address (update a |> Loaded) table
                }

            else
                model

        _ ->
            model


updateEntityIf : (Entity.Entity -> Bool) -> (Entity.Entity -> Entity.Entity) -> Model -> Model
updateEntityIf predicate update model =
    case model.type_ of
        Entity (Loaded a) table ->
            if predicate a then
                { model
                    | type_ = Entity (update a |> Loaded) table
                }

            else
                model

        _ ->
            model


updateUserTags : List Tag.UserTag -> Model -> Model
updateUserTags tags model =
    case model.type_ of
        UserTags table ->
            { model
                | type_ =
                    table
                        |> setData tags
                        |> UserTags
            }

        _ ->
            model


showAddressTxsUtxo : { currency : String, address : String } -> Api.Data.AddressTxs -> Model -> Model
showAddressTxsUtxo id data model =
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
                                Just (AddressTxsUtxoTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddressTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddressTxsUtxoTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showAddressTxsAccount : { currency : String, address : String } -> Api.Data.AddressTxs -> Model -> Model
showAddressTxsAccount id data model =
    let
        addressTxs =
            data.addressTxs
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.AddressTxTxAccount tx_ ->
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
                                Just (AddressTxsAccountTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddressTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsAccountTable
                                        |> Just
                }

        _ ->
            model


showAddresslinkTxsUtxo : { currency : String, source : String, target : String } -> Api.Data.Links -> Model -> Model
showAddresslinkTxsUtxo { currency, source, target } data model =
    let
        addressTxs =
            data.links
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.LinkLinkUtxo tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Addresslink src link table ->
            if Id.addressId src.id /= source || Id.addressId link.node.id /= target || Id.currency src.id /= currency then
                model

            else
                { model
                    | type_ =
                        Addresslink src link <|
                            case table of
                                Just (AddresslinkTxsUtxoTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddresslinkTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddresslinkTxsUtxoTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showAddresslinkTxsAccount : { currency : String, source : String, target : String } -> Api.Data.Links -> Model -> Model
showAddresslinkTxsAccount { currency, source, target } data model =
    let
        addressTxs =
            data.links
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.LinkTxAccount tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Addresslink src tgt table ->
            if Id.addressId src.id /= source || Id.addressId tgt.node.id /= target || Id.currency src.id /= currency then
                model

            else
                { model
                    | type_ =
                        Addresslink src tgt <|
                            case table of
                                Just (AddresslinkTxsAccountTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddresslinkTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsAccountTable
                                        |> Just
                }

        _ ->
            model


showEntitylinkTxsUtxo : { currency : String, source : Int, target : Int } -> Api.Data.Links -> Model -> Model
showEntitylinkTxsUtxo { currency, source, target } data model =
    let
        addressTxs =
            data.links
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.LinkLinkUtxo tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Entitylink src link table ->
            if Id.entityId src.id /= source || Id.entityId link.node.id /= target || Id.currency src.id /= currency then
                model

            else
                { model
                    | type_ =
                        Entitylink src link <|
                            case table of
                                Just (AddresslinkTxsUtxoTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddresslinkTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddresslinkTxsUtxoTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showEntitylinkTxsAccount : { currency : String, source : Int, target : Int } -> Api.Data.Links -> Model -> Model
showEntitylinkTxsAccount { currency, source, target } data model =
    let
        addressTxs =
            data.links
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.LinkTxAccount tx_ ->
                                Just tx_

                            _ ->
                                Nothing
                    )
    in
    case model.type_ of
        Entitylink src tgt table ->
            if Id.entityId src.id /= source || Id.entityId tgt.node.id /= target || Id.currency src.id /= currency then
                model

            else
                { model
                    | type_ =
                        Entitylink src tgt <|
                            case table of
                                Just (AddresslinkTxsAccountTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> AddresslinkTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsAccountTable
                                        |> Just
                }

        _ ->
            model


showAddressTags : { currency : String, address : String } -> Api.Data.AddressTags -> Model -> Model
showAddressTags id data model =
    let
        getUserTag load =
            case load of
                Loaded a ->
                    a.userTag
                        |> Maybe.map
                            (Tag.userTagToApiTag
                                { address = a.address.address
                                , entity = a.address.entity
                                , currency = a.address.currency
                                }
                                False
                                >> List.singleton
                            )
                        |> Maybe.withDefault []

                Loading _ _ ->
                    []
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
                                Just (AddressTagsTable t) ->
                                    let
                                        addressTags =
                                            if List.isEmpty t.data then
                                                getUserTag loadable ++ data.addressTags

                                            else
                                                data.addressTags
                                    in
                                    appendData data.nextPage addressTags t
                                        |> AddressTagsTable
                                        |> Just

                                _ ->
                                    AddressTagsTable.init
                                        |> s_data (getUserTag loadable ++ data.addressTags)
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


showEntityTxsUtxo : { currency : String, entity : Int } -> Api.Data.AddressTxs -> Model -> Model
showEntityTxsUtxo id data model =
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
                                Just (EntityTxsUtxoTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> EntityTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddressTxsUtxoTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> EntityTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showEntityTxsAccount : { currency : String, entity : Int } -> Api.Data.AddressTxs -> Model -> Model
showEntityTxsAccount id data model =
    let
        addressTxs =
            data.addressTxs
                |> List.filterMap
                    (\tx ->
                        case tx of
                            Api.Data.AddressTxTxAccount tx_ ->
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
                                Just (EntityTxsAccountTable t) ->
                                    appendData data.nextPage addressTxs t
                                        |> EntityTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> s_data addressTxs
                                        |> s_nextpage data.nextPage
                                        |> EntityTxsAccountTable
                                        |> Just
                }

        _ ->
            model


showEntityAddressTags : { currency : String, entity : Int } -> Api.Data.AddressTags -> Model -> Model
showEntityAddressTags id data model =
    let
        getUserTag load =
            case load of
                Loaded a ->
                    a.userTag
                        |> Maybe.map
                            (Tag.userTagToApiTag
                                { address = a.entity.rootAddress
                                , entity = a.entity.entity
                                , currency = a.entity.currency
                                }
                                False
                                >> List.singleton
                            )
                        |> Maybe.withDefault []

                Loading _ _ ->
                    []
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
                                Just (EntityTagsTable t) ->
                                    let
                                        addressTags =
                                            if List.isEmpty t.data then
                                                getUserTag loadable ++ data.addressTags

                                            else
                                                data.addressTags
                                    in
                                    appendData data.nextPage addressTags t
                                        |> EntityTagsTable
                                        |> Just

                                _ ->
                                    AddressTagsTable.init
                                        |> s_data (getUserTag loadable ++ data.addressTags)
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


matchTxId : { currency : String, txHash : String } -> Loadable String { a | currency : String, txHash : String } -> Bool
matchTxId { currency, txHash } loadable =
    case loadable of
        Loading c id ->
            c == currency && id == txHash

        Loaded a ->
            a.currency == currency && a.txHash == txHash


matchBlockId : { currency : String, block : Int } -> Loadable Int Api.Data.Block -> Bool
matchBlockId { currency, block } loadable =
    case loadable of
        Loading c id ->
            c == currency && id == block

        Loaded a ->
            a.currency == currency && a.height == block


tableNewState : Table.State -> Model -> Model
tableNewState state model =
    { model
        | type_ =
            case model.type_ of
                Address loadable table ->
                    Address loadable <|
                        case table of
                            Just (AddressTxsUtxoTable t) ->
                                { t | state = state }
                                    |> AddressTxsUtxoTable
                                    |> Just

                            Just (AddressTxsAccountTable t) ->
                                { t | state = state }
                                    |> AddressTxsAccountTable
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

                            Just (EntityTxsUtxoTable t) ->
                                { t | state = state }
                                    |> EntityTxsUtxoTable
                                    |> Just

                            Just (EntityTxsAccountTable t) ->
                                { t | state = state }
                                    |> EntityTxsAccountTable
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

                TxUtxo loadable table ->
                    TxUtxo loadable <|
                        case table of
                            Just (TxUtxoInputsTable t) ->
                                { t | state = state }
                                    |> TxUtxoInputsTable
                                    |> Just

                            Just (TxUtxoOutputsTable t) ->
                                { t | state = state }
                                    |> TxUtxoOutputsTable
                                    |> Just

                            Nothing ->
                                table

                TxAccount _ ->
                    model.type_

                None ->
                    model.type_

                Label label t ->
                    { t | state = state }
                        |> Label label

                Block loadable table ->
                    Block loadable <|
                        case table of
                            Just (BlockTxsUtxoTable t) ->
                                { t | state = state }
                                    |> BlockTxsUtxoTable
                                    |> Just

                            Just (BlockTxsAccountTable t) ->
                                { t | state = state }
                                    |> BlockTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                Addresslink src lnk table ->
                    Addresslink src lnk <|
                        case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                { t | state = state }
                                    |> AddresslinkTxsUtxoTable
                                    |> Just

                            Just (AddresslinkTxsAccountTable t) ->
                                { t | state = state }
                                    |> AddresslinkTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                Entitylink src lnk table ->
                    Entitylink src lnk <|
                        case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                { t | state = state }
                                    |> AddresslinkTxsUtxoTable
                                    |> Just

                            Just (AddresslinkTxsAccountTable t) ->
                                { t | state = state }
                                    |> AddresslinkTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                UserTags t ->
                    { t | state = state }
                        |> UserTags

                Plugin ->
                    model.type_
    }


showPlugin : Model -> Model
showPlugin model =
    show model
        |> s_type_ Plugin


setHeight : Float -> Model -> Model
setHeight height browser =
    { browser
        | height = Just height
    }


infiniteScroll : ScrollPos -> Model -> ( Model, List Effect )
infiniteScroll { scrollTop, contentHeight, containerHeight } model =
    let
        excessHeight =
            contentHeight - containerHeight

        needMore =
            scrollTop >= toFloat (excessHeight - 50)

        wrap t tag effect =
            if not t.loading && t.nextpage /= Nothing then
                ( { t | loading = True }
                    |> tag
                    |> Just
                , effect t.nextpage
                    |> List.singleton
                )

            else
                t |> tag |> Just |> n

        ( type_, eff ) =
            if not needMore then
                n model.type_

            else
                case model.type_ of
                    Address loadable table ->
                        (case table of
                            Just (AddressTxsUtxoTable t) ->
                                loadableAddress loadable
                                    |> getAddressTxsEffect
                                    |> wrap t AddressTxsUtxoTable

                            Just (AddressTxsAccountTable t) ->
                                loadableAddress loadable
                                    |> getAddressTxsEffect
                                    |> wrap t AddressTxsAccountTable

                            Just (AddressTagsTable t) ->
                                loadableAddress loadable
                                    |> getAddressTagsEffect
                                    |> wrap t AddressTagsTable

                            Just (AddressIncomingNeighborsTable t) ->
                                loadableAddress loadable
                                    |> getAddressNeighborsEffect False
                                    |> wrap t AddressIncomingNeighborsTable

                            Just (AddressOutgoingNeighborsTable t) ->
                                loadableAddress loadable
                                    |> getAddressNeighborsEffect True
                                    |> wrap t AddressOutgoingNeighborsTable

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Address loadable)

                    Entity loadable table ->
                        (case table of
                            Just (EntityAddressesTable t) ->
                                loadableEntity loadable
                                    |> getEntityAddressesEffect
                                    |> wrap t EntityAddressesTable

                            Just (EntityTxsUtxoTable t) ->
                                loadableEntity loadable
                                    |> getEntityTxsEffect
                                    |> wrap t EntityTxsUtxoTable

                            Just (EntityTxsAccountTable t) ->
                                loadableEntity loadable
                                    |> getEntityTxsEffect
                                    |> wrap t EntityTxsAccountTable

                            Just (EntityTagsTable t) ->
                                loadableEntity loadable
                                    |> getEntityAddressTagsEffect
                                    |> wrap t EntityTagsTable

                            Just (EntityIncomingNeighborsTable t) ->
                                loadableEntity loadable
                                    |> getEntityNeighborsEffect False
                                    |> wrap t EntityIncomingNeighborsTable

                            Just (EntityOutgoingNeighborsTable t) ->
                                loadableEntity loadable
                                    |> getEntityNeighborsEffect True
                                    |> wrap t EntityOutgoingNeighborsTable

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Entity loadable)

                    Block loadable table ->
                        (case table of
                            Just (BlockTxsUtxoTable t) ->
                                loadableBlock loadable
                                    |> getBlockTxsEffect
                                    |> wrap t BlockTxsUtxoTable

                            Just (BlockTxsAccountTable t) ->
                                loadableBlock loadable
                                    |> getBlockTxsEffect
                                    |> wrap t BlockTxsAccountTable

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Block loadable)

                    TxUtxo _ _ ->
                        ( model.type_, [] )

                    TxAccount _ ->
                        ( model.type_, [] )

                    Label label t ->
                        if not t.loading && t.nextpage /= Nothing then
                            ( { t | loading = True }
                                |> Label label
                            , listAddressTagsEffect label t.nextpage
                                |> List.singleton
                            )

                        else
                            Label label t |> n

                    Addresslink src link table ->
                        let
                            id =
                                { currency = Id.currency src.id
                                , source = Id.addressId src.id
                                , target = Id.addressId link.node.id
                                }
                        in
                        (case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                getAddresslinkTxsEffect id
                                    |> wrap t AddresslinkTxsUtxoTable

                            Just (AddresslinkTxsAccountTable t) ->
                                getAddresslinkTxsEffect id
                                    |> wrap t AddresslinkTxsAccountTable

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Addresslink src link)

                    Entitylink src link table ->
                        let
                            id =
                                { currency = Id.currency src.id
                                , source = Id.entityId src.id
                                , target = Id.entityId link.node.id
                                }
                        in
                        (case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                getEntitylinkTxsEffect id
                                    |> wrap t AddresslinkTxsUtxoTable

                            Just (AddresslinkTxsAccountTable t) ->
                                getEntitylinkTxsEffect id
                                    |> wrap t AddresslinkTxsAccountTable

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Entitylink src link)

                    UserTags _ ->
                        ( model.type_, [] )

                    Plugin ->
                        ( model.type_, [] )

                    None ->
                        ( model.type_, [] )
    in
    ( { model
        | type_ = type_
      }
    , eff
    )


getAddressTxsEffect : A.Address -> Maybe String -> Effect
getAddressTxsEffect { currency, address } nextpage =
    GetAddressTxsEffect
        { currency = currency
        , address = address
        , nextpage = nextpage
        , pagesize = 100
        }
        (BrowserGotAddressTxs { currency = currency, address = address })
        |> ApiEffect


getAddresslinkTxsEffect : A.Addresslink -> Maybe String -> Effect
getAddresslinkTxsEffect id nextpage =
    GetAddresslinkTxsEffect
        { currency = id.currency
        , source = id.source
        , target = id.target
        , nextpage = nextpage
        , pagesize = 100
        }
        (BrowserGotAddresslinkTxs id)
        |> ApiEffect


getEntitylinkTxsEffect : E.Entitylink -> Maybe String -> Effect
getEntitylinkTxsEffect id nextpage =
    GetEntitylinkTxsEffect
        { currency = id.currency
        , source = id.source
        , target = id.target
        , nextpage = nextpage
        , pagesize = 100
        }
        (BrowserGotEntitylinkTxs id)
        |> ApiEffect


getAddressTagsEffect : A.Address -> Maybe String -> Effect
getAddressTagsEffect { currency, address } nextpage =
    GetAddressTagsEffect
        { currency = currency
        , address = address
        , pagesize = 100
        , nextpage = nextpage
        }
        (BrowserGotAddressTagsTable
            { currency = currency
            , address = address
            }
        )
        |> ApiEffect


getAddressNeighborsEffect : Bool -> A.Address -> Maybe String -> Effect
getAddressNeighborsEffect isOutgoing { currency, address } nextpage =
    GetAddressNeighborsEffect
        { currency = currency
        , address = address
        , isOutgoing = isOutgoing
        , pagesize = 100
        , includeLabels = True
        , onlyIds = Nothing
        , nextpage = nextpage
        }
        (BrowserGotAddressNeighborsTable
            { currency = currency
            , address = address
            }
            isOutgoing
        )
        |> ApiEffect


getEntityAddressTagsEffect : E.Entity -> Maybe String -> Effect
getEntityAddressTagsEffect { currency, entity } nextpage =
    GetEntityAddressTagsEffect
        { currency = currency
        , entity = entity
        , pagesize = 100
        , nextpage = nextpage
        }
        (BrowserGotEntityAddressTagsTable
            { currency = currency
            , entity = entity
            }
        )
        |> ApiEffect


getEntityTxsEffect : E.Entity -> Maybe String -> Effect
getEntityTxsEffect { currency, entity } nextpage =
    GetEntityTxsEffect
        { currency = currency
        , entity = entity
        , nextpage = nextpage
        , pagesize = 100
        }
        (BrowserGotEntityTxs { currency = currency, entity = entity })
        |> ApiEffect


getEntityNeighborsEffect : Bool -> E.Entity -> Maybe String -> Effect
getEntityNeighborsEffect isOutgoing { currency, entity } nextpage =
    GetEntityNeighborsEffect
        { currency = currency
        , entity = entity
        , isOutgoing = isOutgoing
        , onlyIds = Nothing
        , pagesize = 100
        , includeLabels = True
        , nextpage = nextpage
        }
        (BrowserGotEntityNeighborsTable
            { currency = currency
            , entity = entity
            }
            isOutgoing
        )
        |> ApiEffect


getEntityAddressesEffect : E.Entity -> Maybe String -> Effect
getEntityAddressesEffect { currency, entity } nextpage =
    GetEntityAddressesEffect
        { currency = currency
        , entity = entity
        , nextpage = nextpage
        , pagesize = 100
        }
        (BrowserGotEntityAddressesForTable { currency = currency, entity = entity })
        |> ApiEffect


showTxUtxoAddresses : { currency : String, txHash : String } -> Bool -> List Api.Data.TxValue -> Model -> Model
showTxUtxoAddresses id isOutgoing data model =
    case model.type_ of
        TxUtxo loadable table ->
            if matchTxId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        TxUtxoTable.init isOutgoing
                            |> appendData Nothing data
                            |> (if isOutgoing then
                                    TxUtxoOutputsTable

                                else
                                    TxUtxoInputsTable
                               )
                            |> Just
                            |> TxUtxo
                                (case loadable of
                                    Loaded tx ->
                                        (if isOutgoing then
                                            { tx
                                                | outputs = Just data
                                            }

                                         else
                                            { tx
                                                | inputs = Just data
                                            }
                                        )
                                            |> Loaded

                                    Loading _ _ ->
                                        loadable
                                )
                }

        _ ->
            model


listAddressTagsEffect : String -> Maybe String -> Effect
listAddressTagsEffect label nextpage =
    ListAddressTagsEffect
        { label = label
        , pagesize = Nothing
        , nextpage = nextpage
        }
        (BrowserGotLabelAddressTags label)
        |> ApiEffect


getBlockTxsEffect : B.Block -> Maybe String -> Effect
getBlockTxsEffect { currency, block } nextpage =
    GetBlockTxsEffect
        { currency = currency
        , block = block
        , nextpage = nextpage
        , pagesize = 100
        }
        (BrowserGotBlockTxs { currency = currency, block = block })
        |> ApiEffect


filterTable : Maybe String -> Model -> Model
filterTable filter model =
    { model
        | type_ =
            case model.type_ of
                Address loadable table ->
                    Address loadable <|
                        case table of
                            Just (AddressTxsUtxoTable t) ->
                                applyFilter filter t
                                    |> AddressTxsUtxoTable
                                    |> Just

                            Just (AddressTxsAccountTable t) ->
                                applyFilter filter t
                                    |> AddressTxsAccountTable
                                    |> Just

                            Just (AddressTagsTable t) ->
                                applyFilter filter t
                                    |> AddressTagsTable
                                    |> Just

                            Just (AddressIncomingNeighborsTable t) ->
                                applyFilter filter t
                                    |> AddressIncomingNeighborsTable
                                    |> Just

                            Just (AddressOutgoingNeighborsTable t) ->
                                applyFilter filter t
                                    |> AddressOutgoingNeighborsTable
                                    |> Just

                            Nothing ->
                                table

                Entity loadable table ->
                    Entity loadable <|
                        case table of
                            Just (EntityAddressesTable t) ->
                                applyFilter filter t
                                    |> EntityAddressesTable
                                    |> Just

                            Just (EntityTxsUtxoTable t) ->
                                applyFilter filter t
                                    |> EntityTxsUtxoTable
                                    |> Just

                            Just (EntityTxsAccountTable t) ->
                                applyFilter filter t
                                    |> EntityTxsAccountTable
                                    |> Just

                            Just (EntityTagsTable t) ->
                                applyFilter filter t
                                    |> EntityTagsTable
                                    |> Just

                            Just (EntityIncomingNeighborsTable t) ->
                                applyFilter filter t
                                    |> EntityIncomingNeighborsTable
                                    |> Just

                            Just (EntityOutgoingNeighborsTable t) ->
                                applyFilter filter t
                                    |> EntityOutgoingNeighborsTable
                                    |> Just

                            Nothing ->
                                table

                TxUtxo loadable table ->
                    TxUtxo loadable <|
                        case table of
                            Just (TxUtxoInputsTable t) ->
                                applyFilter filter t
                                    |> TxUtxoInputsTable
                                    |> Just

                            Just (TxUtxoOutputsTable t) ->
                                applyFilter filter t
                                    |> TxUtxoOutputsTable
                                    |> Just

                            Nothing ->
                                table

                TxAccount _ ->
                    model.type_

                None ->
                    model.type_

                Label label t ->
                    applyFilter filter t
                        |> Label label

                Block loadable table ->
                    Block loadable <|
                        case table of
                            Just (BlockTxsUtxoTable t) ->
                                applyFilter filter t
                                    |> BlockTxsUtxoTable
                                    |> Just

                            Just (BlockTxsAccountTable t) ->
                                applyFilter filter t
                                    |> BlockTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                Addresslink src lnk table ->
                    Addresslink src lnk <|
                        case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                applyFilter filter t
                                    |> AddresslinkTxsUtxoTable
                                    |> Just

                            Just (AddresslinkTxsAccountTable t) ->
                                applyFilter filter t
                                    |> AddresslinkTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                Entitylink src lnk table ->
                    Entitylink src lnk <|
                        case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                applyFilter filter t
                                    |> AddresslinkTxsUtxoTable
                                    |> Just

                            Just (AddresslinkTxsAccountTable t) ->
                                applyFilter filter t
                                    |> AddresslinkTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                UserTags t ->
                    applyFilter filter t
                        |> UserTags

                Plugin ->
                    model.type_
    }
