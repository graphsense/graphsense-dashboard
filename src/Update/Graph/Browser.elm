module Update.Graph.Browser exposing (filterTable, hideTable, infiniteScroll, loadingActor, loadingAddress, loadingBlock, loadingEntity, loadingLabel, loadingTxAccount, loadingTxUtxo, openActor, searchTable, setHeight, showActor, showActorTable, showActorTags, showAddress, showAddressNeighbors, showAddressTable, showAddressTags, showAddressTxsAccount, showAddressTxsUtxo, showAddresslink, showAddresslinkTable, showAddresslinkTxsAccount, showAddresslinkTxsUtxo, showBlock, showBlockTable, showBlockTxsAccount, showBlockTxsUtxo, showEntity, showEntityAddressTags, showEntityAddresses, showEntityNeighbors, showEntityTable, showEntityTxsAccount, showEntityTxsUtxo, showEntitylink, showEntitylinkTable, showEntitylinkTxsAccount, showEntitylinkTxsUtxo, showLabelAddressTags, showPlugin, showTokenTxs, showTx, showTxAccountTable, showTxUtxoAddresses, showTxUtxoTable, showUserTags, tableAsCSV, tableNewState, updateAddress, updateEntityIf, updateUserTags)

import Api.Data
import Config.Graph as Graph
import Config.Update
import Dict
import Effect.Api exposing (Effect(..))
import Effect.Graph exposing (Effect(..))
import Init.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import Init.Graph.Table.AddressTagsTable as AddressTagsTable
import Init.Graph.Table.AddressTxsUtxoTable as AddressTxsUtxoTable
import Init.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import Init.Graph.Table.AllAssetsTable as AllAssetsTable
import Init.Graph.Table.EntityAddressesTable as EntityAddressesTable
import Init.Graph.Table.EntityNeighborsTable as EntityNeighborsTable
import Init.Graph.Table.LabelAddressTagsTable as LabelAddressTagsTable
import Init.Graph.Table.LinksTable as LinksTable
import Init.Graph.Table.TxUtxoTable as TxUtxoTable
import Init.Graph.Table.TxsAccountTable as TxsAccountTable
import Init.Graph.Table.TxsUtxoTable as TxsUtxoTable
import Init.Graph.Table.UserAddressTagsTable as UserAddressTagsTable
import Log
import Model.Actor as Act
import Model.Address as A
import Model.Block as B
import Model.Currency exposing (asset, tokensToValue)
import Model.Entity as E
import Model.Graph.Actor as Actor
import Model.Graph.Address as Address
import Model.Graph.Browser exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Model.Graph.Link as Link exposing (Link, LinkData)
import Model.Graph.Table exposing (..)
import Model.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import Model.Graph.Table.AddressTagsTable as AddressTagsTable
import Model.Graph.Table.AddressTxsUtxoTable as AddressTxsUtxoTable
import Model.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import Model.Graph.Table.AllAssetsTable as AllAssetsTable
import Model.Graph.Table.EntityAddressesTable as EntityAddressesTable
import Model.Graph.Table.EntityNeighborsTable as EntityNeighborsTable
import Model.Graph.Table.LabelAddressTagsTable as LabelAddressTagsTable
import Model.Graph.Table.LinksTable as LinksTable
import Model.Graph.Table.TxUtxoTable as TxUtxoTable
import Model.Graph.Table.TxsAccountTable as TxsAccountTable
import Model.Graph.Table.TxsUtxoTable as TxsUtxoTable
import Model.Graph.Table.UserAddressTagsTable as UserAddressTagsTable
import Model.Graph.Tag as Tag
import Model.Loadable as Loadable exposing (Loadable(..))
import Model.Locale as Locale
import Model.Tx as T
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Route.Graph as Route
import Table
import Tuple exposing (..)
import Update.Graph.Table exposing (UpdateSearchTerm(..), appendData, searchData, setData)
import Util exposing (n)
import Util.Data as Data
import Util.ExternalLinks exposing (addProtocolPrefx, getFontAwesomeIconForUris)
import View.Graph.Label as Label
import View.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import View.Graph.Table.AddressTagsTable as AddressTagsTable
import View.Graph.Table.AddressTxsUtxoTable as AddressTxsUtxoTable
import View.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import View.Graph.Table.AllAssetsTable as AllAssetsTable
import View.Graph.Table.EntityAddressesTable as EntityAddressesTable
import View.Graph.Table.EntityNeighborsTable as EntityNeighborsTable
import View.Graph.Table.LabelAddressTagsTable as LabelAddressTagsTable
import View.Graph.Table.LinksTable as LinksTable
import View.Graph.Table.TxUtxoTable as TxUtxoTable
import View.Graph.Table.TxsAccountTable as TxsAccountTable
import View.Graph.Table.TxsUtxoTable as TxsUtxoTable
import View.Graph.Table.UserAddressTagsTable as UserAddressTagsTable
import View.Locale as Locale


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


loadingBlock : { currency : String, block : Int } -> Model -> ( Model, List Effect )
loadingBlock id model =
    let
        return table =
            ( Block (Loading id.currency id.block) table
            , [ BrowserGotBlock
                    |> GetBlockEffect
                        { height = id.block
                        , currency = id.currency
                        }
                    |> ApiEffect
              ]
            )

        ( type_, eff ) =
            case model.type_ of
                Block loadable table ->
                    if matchBlockId id loadable then
                        ( model.type_, [] )

                    else
                        return table

                _ ->
                    return Nothing
    in
    ( { model
        | type_ = type_
        , visible = True
      }
    , eff
    )


loadingTxAccount : T.TxAccount -> String -> Model -> ( Model, List Effect )
loadingTxAccount id accountCurrency model =
    let
        return table =
            ( TxAccount (Loading id.currency ( id.txHash, id.tokenTxId )) accountCurrency table
            , [ GetTxEffect
                    { txHash = id.txHash
                    , currency = id.currency
                    , tokenTxId = id.tokenTxId
                    , includeIo = False
                    }
                    (BrowserGotTx accountCurrency)
                    |> ApiEffect
              ]
            )

        ( type_, eff ) =
            case model.type_ of
                TxAccount loadable _ table ->
                    if matchTxAccountId id loadable then
                        ( model.type_, [] )

                    else
                        return table

                _ ->
                    return Nothing
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
        return table =
            ( TxUtxo (Loading id.currency id.txHash) table
            , [ GetTxEffect
                    { txHash = id.txHash
                    , currency = id.currency
                    , tokenTxId = Nothing
                    , includeIo = False
                    }
                    (BrowserGotTx id.currency)
                    |> ApiEffect
              ]
            )

        ( type_, eff ) =
            case model.type_ of
                TxUtxo loadable table ->
                    if matchTxId id loadable then
                        ( model.type_, [] )

                    else
                        return table

                _ ->
                    return Nothing
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


loadingActor : String -> Model -> Model
loadingActor actorId model =
    { model
        | type_ = Actor (Loading actorId actorId) Nothing
        , visible = True
    }


openActor : Bool -> Model -> Model
openActor open model =
    { model | visible = open }


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
                |> appendData UserAddressTagsTable.filter tags
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
                        appendData LabelAddressTagsTable.filter data.addressTags table
                            |> s_nextpage data.nextPage
                            |> Label current
                }

        _ ->
            model


showActorTags : String -> Api.Data.AddressTags -> Model -> Model
showActorTags actorId data model =
    case model.type_ of
        Actor current mtable ->
            case mtable of
                Just (ActorTagsTable table) ->
                    { model
                        | type_ =
                            appendData LabelAddressTagsTable.filter data.addressTags table
                                |> s_nextpage data.nextPage
                                |> ActorTagsTable
                                |> Just
                                |> Actor current
                    }

                _ ->
                    model

        _ ->
            model


showAddressTable : Route.AddressTable -> Model -> ( Model, List Effect )
showAddressTable route model =
    case model.type_ of
        Address loadable t ->
            createAddressTable route loadable t
                |> mapFirst (Address loadable)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createAddressTable : Route.AddressTable -> Loadable String Address.Address -> Maybe AddressTable -> ( Maybe AddressTable, List Effect )
createAddressTable route loadable t =
    let
        ( currency, address ) =
            case loadable of
                Loading curr addr ->
                    ( curr, addr )

                Loaded a ->
                    ( a.address.currency, a.address.address )
    in
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
            if Data.isAccountLike currency then
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
            ( AddressNeighborsTable.init |> AddressIncomingNeighborsTable |> Just
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
            ( AddressNeighborsTable.init |> AddressOutgoingNeighborsTable |> Just
            , [ getAddressNeighborsEffect
                    True
                    { currency = currency
                    , address = address
                    }
                    Nothing
              ]
            )

        ( Route.AddressTotalReceivedAllAssetsTable, _ ) ->
            let
                assets =
                    case loadable of
                        Loaded a ->
                            ( currency, a.address.totalReceived )
                                :: (a.address.totalTokensReceived
                                        |> Maybe.map Dict.toList
                                        |> Maybe.withDefault []
                                   )
                                |> tokensToValue a.address.currency

                        _ ->
                            []
            in
            ( AllAssetsTable.init
                |> appendData AllAssetsTable.filter assets
                |> AddressTotalReceivedAllAssetsTable
                |> Just
            , []
            )

        ( Route.AddressFinalBalanceAllAssetsTable, _ ) ->
            let
                assets =
                    case loadable of
                        Loaded a ->
                            ( currency, a.address.balance )
                                :: (a.address.tokenBalances
                                        |> Maybe.map Dict.toList
                                        |> Maybe.withDefault []
                                   )
                                |> tokensToValue a.address.currency

                        _ ->
                            []
            in
            ( AllAssetsTable.init
                |> appendData AllAssetsTable.filter assets
                |> AddressFinalBalanceAllAssetsTable
                |> Just
            , []
            )


showAddresslinkTable : Route.AddresslinkTable -> Model -> ( Model, List Effect )
showAddresslinkTable route model =
    case model.type_ of
        Addresslink source link t ->
            let
                currency =
                    Id.currency source.id
            in
            createAddresslinkTable route t currency (Id.addressId source.id) (Id.addressId link.node.id) link.link
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
            createEntitylinkTable route t currency (Id.entityId source.id) (Id.entityId link.node.id) link.link
                |> mapFirst (Entitylink source link)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createAddresslinkTable : Route.AddresslinkTable -> Maybe AddresslinkTable -> String -> String -> String -> LinkData -> ( Maybe AddresslinkTable, List Effect )
createAddresslinkTable route t currency source target link =
    case ( route, t ) of
        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsUtxoTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsAccountTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, _ ) ->
            if Data.isAccountLike currency then
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

        ( Route.AddresslinkAllAssetsTable, _ ) ->
            ( createLinkAllAssetsTable currency link |> Just
            , []
            )


createLinkAllAssetsTable : String -> LinkData -> AddresslinkTable
createLinkAllAssetsTable currency link =
    let
        assets =
            (case link of
                Link.LinkData { value, tokenValues } ->
                    ( currency, value )
                        :: (tokenValues
                                |> Maybe.map Dict.toList
                                |> Maybe.withDefault []
                           )

                Link.PlaceholderLinkData ->
                    []
            )
                |> tokensToValue currency
    in
    AllAssetsTable.init
        |> appendData AllAssetsTable.filter assets
        |> AddresslinkAllAssetsTable


createEntitylinkTable : Route.AddresslinkTable -> Maybe AddresslinkTable -> String -> Int -> Int -> LinkData -> ( Maybe AddresslinkTable, List Effect )
createEntitylinkTable route t currency source target link =
    case ( route, t ) of
        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsUtxoTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, Just (AddresslinkTxsAccountTable _) ) ->
            n t

        ( Route.AddresslinkTxsTable, _ ) ->
            if Data.isAccountLike currency then
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

        ( Route.AddresslinkAllAssetsTable, _ ) ->
            ( createLinkAllAssetsTable currency link |> Just
            , []
            )


showEntityTable : Route.EntityTable -> Model -> ( Model, List Effect )
showEntityTable route model =
    case model.type_ |> Log.log "showEntityTable" of
        Entity loadable t ->
            createEntityTable route loadable t
                |> Log.log "table"
                |> mapFirst (Entity loadable)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createEntityTable : Route.EntityTable -> Loadable Int Entity.Entity -> Maybe EntityTable -> ( Maybe EntityTable, List Effect )
createEntityTable route loadable t =
    let
        ( currency, entity ) =
            case loadable of
                Loading curr e ->
                    ( curr, e )

                Loaded a ->
                    ( a.entity.currency, a.entity.entity )
    in
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
            if Data.isAccountLike currency then
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
            ( EntityNeighborsTable.init |> EntityIncomingNeighborsTable |> Just
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
            ( EntityNeighborsTable.init |> EntityOutgoingNeighborsTable |> Just
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

        ( Route.EntityTotalReceivedAllAssetsTable, _ ) ->
            let
                assets =
                    case loadable of
                        Loaded a ->
                            ( currency, a.entity.totalReceived )
                                :: (a.entity.totalTokensReceived
                                        |> Maybe.map Dict.toList
                                        |> Maybe.withDefault []
                                   )
                                |> tokensToValue currency

                        _ ->
                            []
            in
            ( AllAssetsTable.init
                |> appendData AllAssetsTable.filter assets
                |> EntityTotalReceivedAllAssetsTable
                |> Just
            , []
            )

        ( Route.EntityFinalBalanceAllAssetsTable, _ ) ->
            let
                assets =
                    case loadable of
                        Loaded a ->
                            ( currency, a.entity.balance )
                                :: (a.entity.tokenBalances
                                        |> Maybe.map Dict.toList
                                        |> Maybe.withDefault []
                                   )
                                |> tokensToValue currency

                        _ ->
                            []
            in
            ( AllAssetsTable.init
                |> appendData AllAssetsTable.filter assets
                |> EntityFinalBalanceAllAssetsTable
                |> Just
            , []
            )


showActorTable : Route.ActorTable -> Model -> ( Model, List Effect )
showActorTable route model =
    case model.type_ |> Log.log "showActorsTagsTable" of
        Actor loadable t ->
            case loadable of
                Loading _ aId ->
                    createActorTable route t aId
                        |> Log.log "table"
                        |> mapFirst (Actor loadable)
                        |> mapFirst
                            (\type_ -> { model | type_ = type_ })
                        |> mapSecond ((::) GetBrowserElementEffect)

                Loaded a ->
                    changeActorTable route t a
                        |> Log.log "table"
                        |> mapFirst (Actor loadable)
                        |> mapFirst
                            (\type_ -> { model | type_ = type_ })
                        |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createActorTable : Route.ActorTable -> Maybe ActorTable -> String -> ( Maybe ActorTable, List Effect )
createActorTable route t actorId =
    case ( route, t ) of
        ( Route.ActorOtherLinksTable, Just (ActorOtherLinksTable _) ) ->
            n t

        ( Route.ActorOtherLinksTable, _ ) ->
            ( LinksTable.init |> ActorOtherLinksTable |> Just, [] )

        ( Route.ActorTagsTable, Just (ActorTagsTable _) ) ->
            n t

        ( Route.ActorTagsTable, _ ) ->
            ( LabelAddressTagsTable.init |> ActorTagsTable |> Just
            , [ getActorTagsEffect
                    { actorId = actorId
                    }
                    Nothing
              ]
            )


changeActorTable : Route.ActorTable -> Maybe ActorTable -> Actor.Actor -> ( Maybe ActorTable, List Effect )
changeActorTable route t actor =
    case ( route, t ) of
        ( Route.ActorOtherLinksTable, _ ) ->
            let
                otherUrls : List String
                otherUrls =
                    Actor.getUrisWithoutMain actor
                        |> getFontAwesomeIconForUris
                        |> List.filter (\( _, icon ) -> icon == Nothing)
                        |> List.map Tuple.first
                        |> List.map addProtocolPrefx
            in
            ( LinksTable.init |> setData LinksTable.filter otherUrls |> ActorOtherLinksTable |> Just, [] )

        _ ->
            createActorTable route t actor.id


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
            if Data.isAccountLike currency then
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


showTxAccountTable : Route.TxTable -> Model -> ( Model, List Effect )
showTxAccountTable route model =
    case model.type_ of
        TxAccount loadable accountCurrency t ->
            let
                ( currency, txHash, _ ) =
                    case loadable of
                        Loading curr ( tx_, _ ) ->
                            ( curr, tx_, Nothing )

                        Loaded a ->
                            ( a.currency, a.txHash, Just a )
            in
            createTxAccountTable route t currency txHash
                |> mapFirst (TxAccount loadable accountCurrency)
                |> mapFirst
                    (\type_ -> { model | type_ = type_ })
                |> mapSecond ((::) GetBrowserElementEffect)

        _ ->
            n model


createTxAccountTable : Route.TxTable -> Maybe TxAccountTable -> String -> String -> ( Maybe TxAccountTable, List Effect )
createTxAccountTable route t currency txHash =
    case ( route, t ) of
        ( Route.TokenTxsTable, Just (TokenTxsTable _) ) ->
            n t

        ( Route.TokenTxsTable, Nothing ) ->
            if Data.isAccountLike currency then
                ( TxsAccountTable.init |> TokenTxsTable |> Just
                , [ getTokenTxsEffect
                        { currency = currency
                        , txHash = txHash
                        }
                  ]
                )

            else
                n t

        ( Route.TxInputsTable, _ ) ->
            n t

        ( Route.TxOutputsTable, _ ) ->
            n t


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
                        |> appendData TxUtxoTable.filter inputs
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
                        |> appendData TxUtxoTable.filter outputs
                        |> TxUtxoOutputsTable
                        |> Just
                    , []
                    )

        ( Route.TokenTxsTable, _ ) ->
            n t


show : Model -> Model
show model =
    { model
        | visible = True
    }


showEntity : Entity.Entity -> Model -> ( Model, List Effect )
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
        |> getBrowserElement


showAddress : Address.Address -> Model -> ( Model, List Effect )
showAddress address model =
    show model
        |> s_type_
            (Address (Loaded address) <|
                case model.type_ of
                    Address loadable table ->
                        if
                            A.Address
                                (loadableAddressCurrency loadable)
                                (loadableAddressId loadable)
                                |> A.equals (A.Address address.address.currency address.address.address)
                        then
                            table

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> getBrowserElement


showActor : Actor.Actor -> Model -> ( Model, List Effect )
showActor actor model =
    show model
        |> s_type_
            (Actor (Loaded actor) <|
                case model.type_ of
                    Actor loadable table ->
                        if
                            loadableActorId loadable
                                == actor.id
                        then
                            table

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> getBrowserElement


showTx : Api.Data.Tx -> String -> Model -> ( Model, List Effect )
showTx data accountCurrency model =
    show model
        |> s_type_
            (case data of
                Api.Data.TxTxUtxo tx ->
                    TxUtxo (Loaded tx) <|
                        case model.type_ of
                            TxUtxo loadable table ->
                                if
                                    matchTxId
                                        { currency = tx.currency
                                        , txHash = tx.txHash
                                        }
                                        loadable
                                then
                                    table

                                else
                                    Nothing

                            _ ->
                                Nothing

                Api.Data.TxTxAccount tx ->
                    TxAccount (Loaded tx) accountCurrency <|
                        case model.type_ of
                            TxAccount loadable _ table ->
                                if
                                    matchTxAccountId
                                        { currency = tx.network
                                        , txHash = tx.txHash
                                        , tokenTxId = tx.tokenTxId
                                        }
                                        loadable
                                then
                                    table

                                else
                                    Nothing

                            _ ->
                                Nothing
            )
        |> getBrowserElement


showBlock : Api.Data.Block -> Model -> ( Model, List Effect )
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
        |> getBrowserElement


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
        Block loadable _ ->
            if matchBlockId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        TxsUtxoTable.init
                            |> appendData TxsUtxoTable.filter blockTxs
                            |> BlockTxsUtxoTable
                            |> Just
                            |> Block loadable
                }

        _ ->
            model


showBlockTxsAccount : Graph.Config -> { currency : String, block : Int } -> List Api.Data.Tx -> Model -> Model
showBlockTxsAccount gc id data model =
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
        Block loadable _ ->
            if matchBlockId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        TxsAccountTable.init
                            |> appendData (TxsAccountTable.filter gc) blockTxs
                            |> BlockTxsAccountTable
                            |> Just
                            |> Block loadable
                }

        _ ->
            model


showTokenTxs : Graph.Config -> T.Tx -> List Api.Data.TxAccount -> Model -> Model
showTokenTxs gc id data model =
    case model.type_ of
        TxAccount loadable accountCurrency _ ->
            if
                matchTxAccountId
                    { currency = id.currency
                    , txHash = id.txHash
                    , tokenTxId = Nothing
                    }
                    loadable
                    |> not
            then
                model

            else
                { model
                    | type_ =
                        TxsAccountTable.init
                            |> appendData (TxsAccountTable.filter gc) data
                            |> TokenTxsTable
                            |> Just
                            |> TxAccount loadable accountCurrency
                }

        _ ->
            model


updateAddress : A.Address -> (Address.Address -> Address.Address) -> Model -> Model
updateAddress { currency, address } update model =
    case model.type_ of
        Address (Loaded a) table ->
            if A.Address a.address.currency a.address.address |> A.equals (A.Address currency address) then
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
                        |> setData UserAddressTagsTable.filter tags
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
                                    appendData AddressTxsUtxoTable.filter addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddressTxsUtxoTable.init
                                        |> setData AddressTxsUtxoTable.filter addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showAddressTxsAccount : Graph.Config -> { currency : String, address : String } -> Api.Data.AddressTxs -> Model -> Model
showAddressTxsAccount gc id data model =
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
                                    appendData (TxsAccountTable.filter gc) addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> AddressTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> appendData (TxsAccountTable.filter gc) addressTxs
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
                                    appendData AddresslinkTxsUtxoTable.filter addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddresslinkTxsUtxoTable.init
                                        |> setData AddresslinkTxsUtxoTable.filter addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showAddresslinkTxsAccount : Graph.Config -> { currency : String, source : String, target : String } -> Api.Data.Links -> Model -> Model
showAddresslinkTxsAccount gc { currency, source, target } data model =
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
                            let
                                filter =
                                    TxsAccountTable.filter gc
                            in
                            case table of
                                Just (AddresslinkTxsAccountTable t) ->
                                    appendData filter addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> setData filter addressTxs
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
                                    appendData AddresslinkTxsUtxoTable.filter addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddresslinkTxsUtxoTable.init
                                        |> setData AddresslinkTxsUtxoTable.filter addressTxs
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showEntitylinkTxsAccount : Graph.Config -> { currency : String, source : Int, target : Int } -> Api.Data.Links -> Model -> Model
showEntitylinkTxsAccount gc { currency, source, target } data model =
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
                            let
                                filter =
                                    TxsAccountTable.filter gc
                            in
                            case table of
                                Just (AddresslinkTxsAccountTable t) ->
                                    appendData filter addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> AddresslinkTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> setData filter addressTxs
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
                                    appendData AddressTagsTable.filter addressTags t
                                        |> s_nextpage data.nextPage
                                        |> AddressTagsTable
                                        |> Just

                                _ ->
                                    AddressTagsTable.init
                                        |> setData AddressTagsTable.filter (getUserTag loadable ++ data.addressTags)
                                        |> s_nextpage data.nextPage
                                        |> AddressTagsTable
                                        |> Just
                }

        _ ->
            model


showAddressNeighbors : Graph.Config -> { currency : String, address : String } -> Bool -> Api.Data.NeighborAddresses -> Model -> Model
showAddressNeighbors gc id isOutgoing data model =
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
                                    appendData (AddressNeighborsTable.filter gc) data.neighbors t
                                        |> s_nextpage data.nextPage
                                        |> AddressOutgoingNeighborsTable
                                        |> Just

                                ( False, Just (AddressIncomingNeighborsTable t) ) ->
                                    appendData (AddressNeighborsTable.filter gc) data.neighbors t
                                        |> s_nextpage data.nextPage
                                        |> AddressIncomingNeighborsTable
                                        |> Just

                                _ ->
                                    AddressNeighborsTable.init
                                        |> setData (AddressNeighborsTable.filter gc) data.neighbors
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


showEntityNeighbors : Graph.Config -> { currency : String, entity : Int } -> Bool -> Api.Data.NeighborEntities -> Model -> Model
showEntityNeighbors gc id isOutgoing data model =
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
                                    appendData (EntityNeighborsTable.filter gc) data.neighbors t
                                        |> s_nextpage data.nextPage
                                        |> EntityOutgoingNeighborsTable
                                        |> Just

                                ( False, Just (EntityIncomingNeighborsTable t) ) ->
                                    appendData (EntityNeighborsTable.filter gc) data.neighbors t
                                        |> s_nextpage data.nextPage
                                        |> EntityIncomingNeighborsTable
                                        |> Just

                                _ ->
                                    EntityNeighborsTable.init
                                        |> setData (EntityNeighborsTable.filter gc) data.neighbors
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
                                    appendData EntityAddressesTable.filter data.addresses t
                                        |> s_nextpage data.nextPage
                                        |> EntityAddressesTable
                                        |> Just

                                _ ->
                                    EntityAddressesTable.init
                                        |> setData EntityAddressesTable.filter data.addresses
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
                                    appendData AddressTxsUtxoTable.filter addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> EntityTxsUtxoTable
                                        |> Just

                                _ ->
                                    AddressTxsUtxoTable.init
                                        |> setData AddressTxsUtxoTable.filter addressTxs
                                        |> s_nextpage data.nextPage
                                        |> EntityTxsUtxoTable
                                        |> Just
                }

        _ ->
            model


showEntityTxsAccount : Graph.Config -> { currency : String, entity : Int } -> Api.Data.AddressTxs -> Model -> Model
showEntityTxsAccount gc id data model =
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
                                    appendData (TxsAccountTable.filter gc) addressTxs t
                                        |> s_nextpage data.nextPage
                                        |> EntityTxsAccountTable
                                        |> Just

                                _ ->
                                    TxsAccountTable.init
                                        |> setData (TxsAccountTable.filter gc) addressTxs
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
                                    appendData AddressTagsTable.filter addressTags t
                                        |> s_nextpage data.nextPage
                                        |> EntityTagsTable
                                        |> Just

                                _ ->
                                    AddressTagsTable.init
                                        |> setData AddressTagsTable.filter (getUserTag loadable ++ data.addressTags)
                                        |> s_nextpage data.nextPage
                                        |> EntityTagsTable
                                        |> Just
                }

        _ ->
            model


matchAddressId : { currency : String, address : String } -> Loadable String Address.Address -> Bool
matchAddressId addr loadable =
    case loadable of
        Loading c id ->
            A.Address c id
                |> A.equals addr

        Loaded a ->
            A.Address a.address.currency a.address.address
                |> A.equals addr


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


matchTxAccountId : T.TxAccount -> Loadable ( String, Maybe Int ) { a | currency : String, txHash : String, tokenTxId : Maybe Int } -> Bool
matchTxAccountId { currency, txHash, tokenTxId } loadable =
    case loadable of
        Loading c ( id, ttid ) ->
            c == currency && id == txHash && ttid == tokenTxId

        Loaded a ->
            a.currency == currency && a.txHash == txHash && a.tokenTxId == tokenTxId


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

                            Just (AddressTotalReceivedAllAssetsTable t) ->
                                { t | state = state }
                                    |> AddressTotalReceivedAllAssetsTable
                                    |> Just

                            Just (AddressFinalBalanceAllAssetsTable t) ->
                                { t | state = state }
                                    |> AddressTotalReceivedAllAssetsTable
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

                            Just (EntityTotalReceivedAllAssetsTable t) ->
                                { t | state = state }
                                    |> EntityTotalReceivedAllAssetsTable
                                    |> Just

                            Just (EntityFinalBalanceAllAssetsTable t) ->
                                { t | state = state }
                                    |> EntityTotalReceivedAllAssetsTable
                                    |> Just

                            Nothing ->
                                table

                Actor loadable table ->
                    Actor loadable <|
                        case table of
                            Just (ActorTagsTable t) ->
                                { t | state = state }
                                    |> ActorTagsTable
                                    |> Just

                            Just (ActorOtherLinksTable t) ->
                                { t | state = state }
                                    |> ActorOtherLinksTable
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

                TxAccount loadable accountCurrency table ->
                    TxAccount loadable accountCurrency <|
                        case table of
                            Just (TokenTxsTable t) ->
                                { t | state = state }
                                    |> TokenTxsTable
                                    |> Just

                            Nothing ->
                                table

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

                            Just (AddresslinkAllAssetsTable t) ->
                                { t | state = state }
                                    |> AddresslinkAllAssetsTable
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

                            Just (AddresslinkAllAssetsTable t) ->
                                { t | state = state }
                                    |> AddresslinkAllAssetsTable
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

                            Just (AddressTotalReceivedAllAssetsTable t) ->
                                ( table, [] )

                            Just (AddressFinalBalanceAllAssetsTable t) ->
                                ( table, [] )

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

                            Just (EntityTotalReceivedAllAssetsTable t) ->
                                ( table, [] )

                            Just (EntityFinalBalanceAllAssetsTable t) ->
                                ( table, [] )

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Entity loadable)

                    Actor loadable table ->
                        (case table of
                            Just (ActorTagsTable t) ->
                                loadableActor loadable
                                    |> getActorTagsEffect
                                    |> wrap t ActorTagsTable

                            Just (ActorOtherLinksTable _) ->
                                ( table, [] )

                            Nothing ->
                                ( table, [] )
                        )
                            |> mapFirst (Actor loadable)

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

                    TxAccount _ _ _ ->
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

                            Just (AddresslinkAllAssetsTable t) ->
                                ( table, [] )

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

                            Just (AddresslinkAllAssetsTable t) ->
                                ( table, [] )

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
        , direction = Nothing
        , nextpage = nextpage
        , pagesize = 100
        , order = Nothing
        , tokenCurrency = Nothing
        , minHeight = Nothing
        , maxHeight = Nothing
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
        , order = Nothing
        , minHeight = Nothing
        , maxHeight = Nothing
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
        , order = Nothing
        , minHeight = Nothing
        , maxHeight = Nothing
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
        , includeBestClusterTag = False
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
        , includeActors = True
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


getActorTagsEffect : Act.Actor -> Maybe String -> Effect
getActorTagsEffect { actorId } nextpage =
    GetActorTagsEffect
        { actorId = actorId
        , pagesize = 100
        , nextpage = nextpage
        }
        (BrowserGotActorTagsTable
            { actorId = actorId
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
        TxUtxo loadable _ ->
            if matchTxId id loadable |> not then
                model

            else
                { model
                    | type_ =
                        TxUtxoTable.init isOutgoing
                            |> appendData TxUtxoTable.filter data
                            |> (if isOutgoing then
                                    TxUtxoOutputsTable

                                else
                                    TxUtxoInputsTable
                               )
                            |> Just
                            |> TxUtxo
                                (Loadable.map
                                    (\tx ->
                                        if isOutgoing then
                                            { tx
                                                | outputs = Just data
                                            }

                                        else
                                            { tx
                                                | inputs = Just data
                                            }
                                    )
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


getTokenTxsEffect : T.Tx -> Effect
getTokenTxsEffect { currency, txHash } =
    GetTokenTxsEffect
        { currency = currency
        , txHash = txHash
        }
        (BrowserGotTokenTxs { currency = currency, txHash = txHash })
        |> ApiEffect


filterTable : Graph.Config -> Model -> Model
filterTable gc model =
    searchTable gc Keep model


searchTable : Graph.Config -> UpdateSearchTerm -> Model -> Model
searchTable gc searchTerm model =
    { model
        | type_ =
            case model.type_ of
                Address loadable table ->
                    Address loadable <|
                        case table of
                            Just (AddressTxsUtxoTable t) ->
                                searchData AddressTxsUtxoTable.filter searchTerm t
                                    |> AddressTxsUtxoTable
                                    |> Just

                            Just (AddressTxsAccountTable t) ->
                                searchData (TxsAccountTable.filter gc) searchTerm t
                                    |> AddressTxsAccountTable
                                    |> Just

                            Just (AddressTagsTable t) ->
                                searchData AddressTagsTable.filter searchTerm t
                                    |> AddressTagsTable
                                    |> Just

                            Just (AddressIncomingNeighborsTable t) ->
                                searchData (AddressNeighborsTable.filter gc) searchTerm t
                                    |> AddressIncomingNeighborsTable
                                    |> Just

                            Just (AddressOutgoingNeighborsTable t) ->
                                searchData (AddressNeighborsTable.filter gc) searchTerm t
                                    |> AddressOutgoingNeighborsTable
                                    |> Just

                            Just (AddressTotalReceivedAllAssetsTable t) ->
                                searchData AllAssetsTable.filter searchTerm t
                                    |> AddressTotalReceivedAllAssetsTable
                                    |> Just

                            Just (AddressFinalBalanceAllAssetsTable t) ->
                                searchData AllAssetsTable.filter searchTerm t
                                    |> AddressFinalBalanceAllAssetsTable
                                    |> Just

                            Nothing ->
                                table

                Entity loadable table ->
                    Entity loadable <|
                        case table of
                            Just (EntityAddressesTable t) ->
                                searchData EntityAddressesTable.filter searchTerm t
                                    |> EntityAddressesTable
                                    |> Just

                            Just (EntityTxsUtxoTable t) ->
                                searchData AddressTxsUtxoTable.filter searchTerm t
                                    |> EntityTxsUtxoTable
                                    |> Just

                            Just (EntityTxsAccountTable t) ->
                                searchData (TxsAccountTable.filter gc) searchTerm t
                                    |> EntityTxsAccountTable
                                    |> Just

                            Just (EntityTagsTable t) ->
                                searchData AddressTagsTable.filter searchTerm t
                                    |> EntityTagsTable
                                    |> Just

                            Just (EntityIncomingNeighborsTable t) ->
                                searchData (EntityNeighborsTable.filter gc) searchTerm t
                                    |> EntityIncomingNeighborsTable
                                    |> Just

                            Just (EntityOutgoingNeighborsTable t) ->
                                searchData (EntityNeighborsTable.filter gc) searchTerm t
                                    |> EntityOutgoingNeighborsTable
                                    |> Just

                            Just (EntityTotalReceivedAllAssetsTable t) ->
                                searchData AllAssetsTable.filter searchTerm t
                                    |> EntityTotalReceivedAllAssetsTable
                                    |> Just

                            Just (EntityFinalBalanceAllAssetsTable t) ->
                                searchData AllAssetsTable.filter searchTerm t
                                    |> EntityFinalBalanceAllAssetsTable
                                    |> Just

                            Nothing ->
                                table

                Actor loadable table ->
                    Actor loadable <|
                        case table of
                            Just (ActorTagsTable t) ->
                                searchData LabelAddressTagsTable.filter searchTerm t
                                    |> ActorTagsTable
                                    |> Just

                            Just (ActorOtherLinksTable t) ->
                                searchData LinksTable.filter searchTerm t
                                    |> ActorOtherLinksTable
                                    |> Just

                            Nothing ->
                                table

                TxUtxo loadable table ->
                    TxUtxo loadable <|
                        case table of
                            Just (TxUtxoInputsTable t) ->
                                searchData TxUtxoTable.filter searchTerm t
                                    |> TxUtxoInputsTable
                                    |> Just

                            Just (TxUtxoOutputsTable t) ->
                                searchData TxUtxoTable.filter searchTerm t
                                    |> TxUtxoOutputsTable
                                    |> Just

                            Nothing ->
                                table

                TxAccount loadable accountCurrency table ->
                    TxAccount loadable accountCurrency <|
                        case table of
                            Just (TokenTxsTable t) ->
                                searchData (TxsAccountTable.filter gc) searchTerm t
                                    |> TokenTxsTable
                                    |> Just

                            Nothing ->
                                table

                None ->
                    model.type_

                Label label t ->
                    searchData LabelAddressTagsTable.filter searchTerm t
                        |> Label label

                Block loadable table ->
                    Block loadable <|
                        case table of
                            Just (BlockTxsUtxoTable t) ->
                                searchData TxsUtxoTable.filter searchTerm t
                                    |> BlockTxsUtxoTable
                                    |> Just

                            Just (BlockTxsAccountTable t) ->
                                searchData (TxsAccountTable.filter gc) searchTerm t
                                    |> BlockTxsAccountTable
                                    |> Just

                            Nothing ->
                                table

                Addresslink src lnk table ->
                    Addresslink src lnk <|
                        case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                searchData AddresslinkTxsUtxoTable.filter searchTerm t
                                    |> AddresslinkTxsUtxoTable
                                    |> Just

                            Just (AddresslinkTxsAccountTable t) ->
                                searchData (TxsAccountTable.filter gc) searchTerm t
                                    |> AddresslinkTxsAccountTable
                                    |> Just

                            Just (AddresslinkAllAssetsTable t) ->
                                searchData AllAssetsTable.filter searchTerm t
                                    |> AddresslinkAllAssetsTable
                                    |> Just

                            Nothing ->
                                table

                Entitylink src lnk table ->
                    Entitylink src lnk <|
                        case table of
                            Just (AddresslinkTxsUtxoTable t) ->
                                searchData AddresslinkTxsUtxoTable.filter searchTerm t
                                    |> AddresslinkTxsUtxoTable
                                    |> Just

                            Just (AddresslinkTxsAccountTable t) ->
                                searchData (TxsAccountTable.filter gc) searchTerm t
                                    |> AddresslinkTxsAccountTable
                                    |> Just

                            Just (AddresslinkAllAssetsTable t) ->
                                searchData AllAssetsTable.filter searchTerm t
                                    |> AddresslinkAllAssetsTable
                                    |> Just

                            Nothing ->
                                table

                UserTags t ->
                    searchData UserAddressTagsTable.filter searchTerm t
                        |> UserTags

                Plugin ->
                    model.type_
    }


tableAsCSV : Locale.Model -> Config.Update.Config -> Model -> Maybe ( String, String )
tableAsCSV locale uc { type_ } =
    let
        translate =
            --List.map (mapFirst (\( str, params ) -> Locale.interpolated locale str params))
            List.map (mapFirst first)

        asCsv prep t title =
            Update.Graph.Table.asCsv (prep >> translate) t |> pair title |> Just

        loadableAddressToList l =
            loadableAddress l
                |> (\{ address, currency } -> [ address, String.toUpper currency ])

        loadableEntityToList l =
            loadableEntity l
                |> (\{ entity, currency } -> [ String.fromInt entity, String.toUpper currency ])

        loadableBlockToList l =
            loadableBlock l
                |> (\{ block, currency } -> [ String.fromInt block, String.toUpper currency ])

        loadableTxToList t =
            loadableTx t
                |> (\{ txHash, currency } -> [ txHash, String.toUpper currency ])

        loadableTxAccountToList t =
            loadableTxAccount t
                |> (\{ txHash, currency } -> [ txHash, String.toUpper currency ])
    in
    case type_ of
        Address loadable table ->
            case table of
                Just (AddressTxsUtxoTable t) ->
                    loadableAddressToList loadable
                        |> Locale.interpolated locale "Address transactions of {0} ({1})"
                        |> asCsv (AddressTxsUtxoTable.prepareCSV locale (loadableAddressCurrency loadable)) t

                Just (AddressTxsAccountTable t) ->
                    loadableAddressToList loadable
                        |> Locale.interpolated locale "Address transactions of {0} ({1})"
                        |> asCsv (TxsAccountTable.prepareCSV locale (loadableAddressCurrency loadable)) t

                Just (AddressTagsTable _) ->
                    Nothing

                Just (AddressIncomingNeighborsTable t) ->
                    loadableAddressToList loadable
                        |> Locale.interpolated locale "Incoming neighbors of address {0} ({1})"
                        |> asCsv (AddressNeighborsTable.prepareCSV locale False (loadableAddressCurrency loadable)) t

                Just (AddressOutgoingNeighborsTable t) ->
                    loadableAddressToList loadable
                        |> Locale.interpolated locale "Outgoing neighbors of address {0} ({1})"
                        |> asCsv (AddressNeighborsTable.prepareCSV locale True (loadableAddressCurrency loadable)) t

                Just (AddressTotalReceivedAllAssetsTable t) ->
                    loadableAddressToList loadable
                        |> Locale.interpolated locale "Total received assets of address {0} ({1})"
                        |> asCsv (AllAssetsTable.prepareCSV locale (loadableAddressCurrency loadable)) t

                Just (AddressFinalBalanceAllAssetsTable t) ->
                    loadableAddressToList loadable
                        |> Locale.interpolated locale "Final balance assets of address {0} ({1})"
                        |> asCsv (AllAssetsTable.prepareCSV locale (loadableAddressCurrency loadable)) t

                Nothing ->
                    Nothing

        Entity loadable table ->
            case table of
                Just (EntityAddressesTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "addresses of entity {0} ({1})"
                        |> asCsv (EntityAddressesTable.prepareCSV locale (loadableEntityCurrency loadable)) t

                Just (EntityTxsUtxoTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "Address transactions of entity {0} ({1})"
                        |> asCsv (AddressTxsUtxoTable.prepareCSV locale (loadableEntityCurrency loadable)) t

                Just (EntityTxsAccountTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "Address transactions of entity {0} ({1})"
                        |> asCsv (TxsAccountTable.prepareCSV locale (loadableEntityCurrency loadable)) t

                Just (EntityTagsTable _) ->
                    Nothing

                Just (EntityIncomingNeighborsTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "Incoming neighbors of entity {0} ({1})"
                        |> asCsv (EntityNeighborsTable.prepareCSV locale False (loadableEntityCurrency loadable)) t

                Just (EntityOutgoingNeighborsTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "Outgoing neighbors of entity {0} ({1})"
                        |> asCsv (EntityNeighborsTable.prepareCSV locale True (loadableEntityCurrency loadable)) t

                Just (EntityTotalReceivedAllAssetsTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "Total received assets of address {0} ({1})"
                        |> asCsv (AllAssetsTable.prepareCSV locale (loadableEntityCurrency loadable)) t

                Just (EntityFinalBalanceAllAssetsTable t) ->
                    loadableEntityToList loadable
                        |> Locale.interpolated locale "Final balance assets of address {0} ({1})"
                        |> asCsv (AllAssetsTable.prepareCSV locale (loadableEntityCurrency loadable)) t

                Nothing ->
                    Nothing

        Actor _ table ->
            case table of
                Just (ActorTagsTable _) ->
                    Nothing

                Just (ActorOtherLinksTable _) ->
                    Nothing

                Nothing ->
                    Nothing

        TxUtxo loadable table ->
            case table of
                Just (TxUtxoInputsTable t) ->
                    loadableTxToList loadable
                        |> Locale.interpolated locale "Incoming values of transaction {0} ({1})"
                        |> asCsv (TxUtxoTable.prepareCSV locale (loadableCurrency loadable) False) t

                Just (TxUtxoOutputsTable t) ->
                    loadableTxToList loadable
                        |> Locale.interpolated locale "Outgoing values of transaction {0} ({1})"
                        |> asCsv (TxUtxoTable.prepareCSV locale (loadableCurrency loadable) False) t

                Nothing ->
                    Nothing

        TxAccount loadable accountCurrency table ->
            case table of
                Just (TokenTxsTable t) ->
                    loadableTxAccountToList loadable
                        |> Locale.interpolated locale "Token transactions of {0} ({1})"
                        |> asCsv (TxsAccountTable.prepareCSV locale accountCurrency) t

                Nothing ->
                    Nothing

        None ->
            Nothing

        Label _ _ ->
            Nothing

        Block loadable table ->
            case table of
                Just (BlockTxsUtxoTable t) ->
                    loadableBlockToList loadable
                        |> Locale.interpolated locale "Transactions of block {0} ({1})"
                        |> asCsv (TxsUtxoTable.prepareCSV locale (loadableCurrency loadable)) t

                Just (BlockTxsAccountTable t) ->
                    loadableBlockToList loadable
                        |> Locale.interpolated locale "Transactions of block {0} ({1})"
                        |> asCsv (TxsAccountTable.prepareCSV locale (loadableCurrency loadable)) t

                Nothing ->
                    Nothing

        Addresslink src lnk table ->
            let
                currency =
                    String.toUpper src.address.currency

                title prefix =
                    [ src.address.address
                    , lnk.node.address.address
                    , currency
                    ]
                        |> Locale.interpolated locale (prefix ++ " between addresses {0} and {1} ({2})")
            in
            case table of
                Just (AddresslinkTxsUtxoTable t) ->
                    title "Transactions"
                        |> asCsv (AddresslinkTxsUtxoTable.prepareCSV locale currency) t

                Just (AddresslinkTxsAccountTable t) ->
                    title "Transactions"
                        |> asCsv (TxsAccountTable.prepareCSV locale currency) t

                Just (AddresslinkAllAssetsTable t) ->
                    title "Total assets"
                        |> asCsv (AllAssetsTable.prepareCSV locale currency) t

                Nothing ->
                    Nothing

        Entitylink src lnk table ->
            let
                currency =
                    src.entity.currency

                title =
                    [ String.fromInt src.entity.entity
                    , String.fromInt lnk.node.entity.entity
                    , String.toUpper src.entity.currency
                    ]
                        |> Locale.interpolated locale "Transactions between entities {0} and {1} ({2})"
            in
            case table of
                Just (AddresslinkTxsUtxoTable t) ->
                    title |> asCsv (AddresslinkTxsUtxoTable.prepareCSV locale currency) t

                Just (AddresslinkTxsAccountTable t) ->
                    title
                        |> asCsv (TxsAccountTable.prepareCSV locale currency) t

                Just (AddresslinkAllAssetsTable t) ->
                    title
                        |> asCsv (AllAssetsTable.prepareCSV locale currency) t

                Nothing ->
                    Nothing

        UserTags t ->
            Locale.string locale "user address tags"
                |> asCsv (UserAddressTagsTable.prepareCSV uc) t

        Plugin ->
            Nothing


getBrowserElement : Model -> ( Model, List Effect )
getBrowserElement model =
    ( model
    , [ GetBrowserElementEffect ]
    )


hideTable : Model -> Model
hideTable model =
    { model
        | type_ =
            case model.type_ of
                Address a _ ->
                    Address a Nothing

                None ->
                    None

                Entity a _ ->
                    Entity a Nothing

                Actor a _ ->
                    Actor a Nothing

                TxUtxo a _ ->
                    TxUtxo a Nothing

                TxAccount a b _ ->
                    TxAccount a b Nothing

                Label a t ->
                    Label a t

                Block a _ ->
                    Block a Nothing

                Addresslink a b _ ->
                    Addresslink a b Nothing

                Entitylink a b _ ->
                    Entitylink a b Nothing

                UserTags a ->
                    UserTags a

                Plugin ->
                    model.type_
    }
