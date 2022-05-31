module Update.Graph exposing (..)

import Api.Data
import Config.Graph exposing (maxExpandableAddresses, maxExpandableNeighbors)
import Config.Update as Update
import Dict
import Effect exposing (n)
import Effect.Graph exposing (Effect(..), getEntityEgonet)
import Init.Graph.ContextMenu as ContextMenu
import Init.Graph.Id as Id
import IntDict exposing (IntDict)
import Json.Encode exposing (Value)
import List.Extra
import Log
import Maybe.Extra
import Model.Address as A
import Model.Graph exposing (..)
import Model.Graph.Address exposing (Address)
import Model.Graph.Browser as Browser
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id as Id exposing (EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Link as Link
import Msg.Graph as Msg exposing (Msg(..))
import Plugin as Plugin exposing (Plugins)
import Plugin.Model as Plugin
import RecordSetter exposing (..)
import Route as R exposing (toUrl)
import Route.Graph as Route
import Set exposing (Set)
import Tuple exposing (..)
import Update.Graph.Adding as Adding
import Update.Graph.Address as Address
import Update.Graph.Browser as Browser
import Update.Graph.Color as Color
import Update.Graph.Layer as Layer
import Update.Graph.Transform as Transform


addAddress :
    Plugins
    -> Update.Config
    ->
        { address : Api.Data.Address
        , entity : Api.Data.Entity
        , incoming : List Api.Data.NeighborEntity
        , outgoing : List Api.Data.NeighborEntity
        }
    -> Model
    -> ( Model, List Effect )
addAddress plugins uc { address, entity, incoming, outgoing } model =
    let
        ( newModel, _ ) =
            addEntity uc
                { entity = entity
                , incoming = incoming
                , outgoing = outgoing
                }
                model

        added =
            Layer.addAddress plugins uc newModel.config.colors address newModel.layers

        newModel_ =
            { newModel
                | layers =
                    added.layers
                        |> Layer.syncLinks (Debug.log "addAddress.repositioned" added.repositioned)
                , config =
                    newModel.config
                        |> s_colors added.colors
            }

        addedAddress =
            added.new
                |> Set.toList
                |> List.head
                |> Maybe.andThen (\a -> Layer.getAddress a newModel_.layers)

        getTagsEffect =
            GetAddressTagsEffect
                { address = address.address
                , currency = address.currency
                , nextpage = Nothing
                , pagesize = 10
                , toMsg =
                    BrowserGotAddressTags
                        { currency = address.currency
                        , address = address.address
                        }
                }
    in
    addedAddress
        |> Maybe.map (\a -> selectAddress a Nothing newModel_)
        |> Maybe.withDefault (n newModel_)
        |> mapSecond ((::) getTagsEffect)
        |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))


addEntity : Update.Config -> { entity : Api.Data.Entity, incoming : List Api.Data.NeighborEntity, outgoing : List Api.Data.NeighborEntity } -> Model -> ( Model, List Effect )
addEntity uc { entity, incoming, outgoing } model =
    let
        findEntities e =
            (++)
                (Layer.getEntities e.entity.currency e.entity.entity model.layers)

        outgoingAnchors =
            incoming
                |> List.foldl findEntities []
                |> List.map (\e -> ( Id.layer e.id, ( e, True ) ))
                |> IntDict.fromList

        incomingAnchors =
            outgoing
                |> List.foldl findEntities []
                |> List.map (\e -> ( Id.layer e.id, ( e, False ) ))
                |> IntDict.fromList

        added =
            if IntDict.isEmpty outgoingAnchors && IntDict.isEmpty incomingAnchors then
                Layer.addEntity uc model.config.colors entity model.layers

            else
                Layer.addEntitiesAt uc
                    (Layer.anchorsToPositions (Just outgoingAnchors) model.layers)
                    [ entity ]
                    { layers = model.layers
                    , new = Set.empty
                    , colors = model.config.colors
                    , repositioned = Set.empty
                    }
                    |> Layer.addEntitiesAt uc
                        (Layer.anchorsToPositions (Just incomingAnchors) model.layers)
                        [ entity ]

        newModel =
            { model
                | layers = added.layers
                , config =
                    model.config
                        |> s_colors added.colors
            }
                |> addEntityEgonet entity.currency entity.entity True outgoing
                |> addEntityEgonet entity.currency entity.entity False incoming
    in
    added.new
        |> Set.toList
        |> List.head
        |> Maybe.andThen (\e -> Layer.getEntity e added.layers)
        |> Maybe.map (\e -> selectEntity e Nothing newModel)
        |> Maybe.withDefault (n newModel)


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    case Log.truncate "msg" msg of
        InternalGraphAddedAddresses _ ->
            n model

        BrowserGotSvgElement result ->
            result
                |> Result.map
                    (\{ element } ->
                        { model
                            | size =
                                { x = element.width
                                , y = element.height
                                }
                                    |> Just
                        }
                    )
                |> Result.withDefault model
                |> n

        BrowserGotBrowserElement result ->
            result
                |> Result.map
                    (\{ element } ->
                        { model
                            | browser = Browser.setHeight element.height model.browser
                        }
                    )
                |> Result.withDefault model
                |> n

        UserClickedGraph dragging ->
            let
                click =
                    case dragging of
                        NoDragging ->
                            True

                        Dragging _ start current ->
                            draggingToClick start current

                        DraggingNode _ start current ->
                            draggingToClick start current
            in
            if click then
                ( model
                , Route.rootRoute
                    |> NavPushRouteEffect
                    |> List.singleton
                )

            else
                n model

        UserWheeledOnGraph x y z ->
            model.size
                |> Maybe.map
                    (\size ->
                        { model
                            | transform =
                                Transform.wheel
                                    { width = size.x
                                    , height = size.y
                                    }
                                    x
                                    y
                                    z
                                    model.transform
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserPushesLeftMouseButtonOnGraph coords ->
            { model
                | dragging =
                    case model.dragging of
                        NoDragging ->
                            Dragging model.transform coords coords

                        x ->
                            x
            }
                |> n

        UserPushesLeftMouseButtonOnEntity id coords ->
            { model
                | dragging =
                    case model.dragging of
                        NoDragging ->
                            DraggingNode id coords coords

                        x ->
                            x
            }
                |> n

        UserMovesMouseOnGraph coords ->
            (case model.dragging of
                NoDragging ->
                    model

                Dragging transform start _ ->
                    { model
                        | transform = Transform.update start coords transform
                        , dragging = Dragging transform start coords
                    }

                DraggingNode id start _ ->
                    let
                        vector =
                            Transform.vector start coords model.transform
                    in
                    { model
                        | layers =
                            Layer.moveEntity id vector model.layers
                                |> Layer.syncLinks (Set.singleton id)
                        , dragging = DraggingNode id start coords
                    }
            )
                |> n

        UserReleasesMouseButton ->
            (case model.dragging of
                NoDragging ->
                    model

                Dragging _ _ _ ->
                    { model
                        | dragging = NoDragging
                    }

                DraggingNode id _ _ ->
                    { model
                        | layers = Layer.releaseEntity id model.layers
                        , dragging = NoDragging
                    }
            )
                |> n

        UserClickedAddress id ->
            ( model
            , Route.addressRoute
                { currency = Id.currency id
                , address = Id.addressId id
                , table = Nothing
                , layer = Id.layer id |> Just
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserRightClickedAddress id coords ->
            Layer.getAddress id model.layers
                |> Maybe.map
                    (\address ->
                        { model
                            | contextMenu =
                                ContextMenu.initAddress coords address
                                    |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserHoversAddress id ->
            n model

        UserClickedEntity id moved ->
            if draggingToClick { x = 0, y = 0 } moved then
                ( model
                , Route.entityRoute
                    { currency = Id.currency id
                    , entity = Id.entityId id
                    , table = Nothing
                    , layer = Id.layer id |> Just
                    }
                    |> NavPushRouteEffect
                    |> List.singleton
                )

            else
                n model

        UserRightClickedEntity id coords ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\entity ->
                        { model
                            | contextMenu =
                                ContextMenu.initEntity coords entity
                                    |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserHoversEntity id ->
            n model

        UserHoversEntityLink id ->
            { model
                | hovered = HoveredEntityLink id
            }
                |> n

        UserHoversAddressLink id ->
            { model
                | hovered = HoveredAddressLink id
            }
                |> n

        UserLeavesThing ->
            { model
                | hovered = HoveredNone
            }
                |> n

        UserClickedAddressesExpand id ->
            Layer.getEntity id model.layers
                |> Debug.log "entity"
                |> Maybe.map
                    (\entity ->
                        if entity.entity.noAddresses < maxExpandableAddresses then
                            ( model
                            , GetEntityAddressesEffect
                                { currency = Id.currency id
                                , entity = Id.entityId id
                                , pagesize = maxExpandableAddresses
                                , nextpage = Nothing
                                , toMsg = BrowserGotEntityAddresses id
                                }
                            )

                        else
                            ( model
                            , Route.entityRoute
                                { currency = Id.currency id
                                , entity = Id.entityId id
                                , table = Just Route.EntityAddressesTable
                                , layer = Id.layer id |> Just
                                }
                                |> NavPushRouteEffect
                            )
                    )
                |> Maybe.map (mapSecond List.singleton)
                |> Maybe.withDefault (n model)

        UserClickedEntityExpandHandle id isOutgoing ->
            case Layer.getEntity id model.layers of
                Nothing ->
                    n model

                Just entity ->
                    if
                        (isOutgoing && entity.entity.outDegree <= maxExpandableNeighbors)
                            || (not isOutgoing && entity.entity.inDegree <= maxExpandableNeighbors)
                    then
                        ( model
                        , { currency = Id.currency id
                          , entity = Id.entityId id
                          , isOutgoing = isOutgoing
                          , onlyIds = Nothing
                          , pagesize = 20
                          , includeLabels = False
                          , nextpage = Nothing
                          , toMsg = BrowserGotEntityNeighbors id isOutgoing
                          }
                            |> GetEntityNeighborsEffect
                            |> List.singleton
                        )

                    else
                        ( model
                        , Route.entityRoute
                            { currency = Id.currency id
                            , entity = Id.entityId id
                            , table =
                                (if isOutgoing then
                                    Route.EntityOutgoingNeighborsTable

                                 else
                                    Route.EntityIncomingNeighborsTable
                                )
                                    |> Just
                            , layer = Id.layer id |> Just
                            }
                            |> NavPushRouteEffect
                            |> List.singleton
                        )

        BrowserGotAddress address ->
            let
                id =
                    { currency = address.currency, address = address.address }

                adding =
                    Adding.setAddress id address model.adding
            in
            case Adding.readyAddress id adding of
                Nothing ->
                    n { model | adding = adding }

                Just added ->
                    { model | adding = Adding.removeAddress id model.adding }
                        |> addAddress plugins uc added

        BrowserGotEntityForAddress address entity ->
            let
                id =
                    { currency = entity.currency, address = address }

                adding =
                    Adding.setEntityForAddress id entity model.adding
            in
            case Adding.readyAddress id adding of
                Nothing ->
                    ( { model | adding = adding }
                    , getEntityEgonet
                        { currency = entity.currency
                        , entity = entity.entity
                        }
                        (BrowserGotEntityEgonetForAddress address)
                        model.layers
                    )

                Just added ->
                    { model | adding = Adding.removeAddress id model.adding }
                        |> addAddress plugins uc added

        BrowserGotEntity entity ->
            let
                id =
                    { currency = entity.currency
                    , entity = entity.entity
                    }

                adding =
                    Adding.setEntityForEntity id entity model.adding
            in
            case Adding.readyEntity id adding of
                Nothing ->
                    n { model | adding = adding }

                Just added ->
                    { model | adding = Adding.removeEntity id model.adding }
                        |> addEntity uc added

        BrowserGotEntityNeighbors id isOutgoing neighbors ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\anchor ->
                        handleEntityNeighbors uc anchor isOutgoing neighbors.neighbors model
                    )
                |> Maybe.withDefault (n model)

        BrowserGotEntityEgonet currency id isOutgoing neighbors ->
            let
                e =
                    { currency = currency, entity = id }

                adding =
                    (if isOutgoing then
                        Adding.setOutgoingForEntity

                     else
                        Adding.setIncomingForEntity
                    )
                        e
                        neighbors.neighbors
                        model.adding
            in
            case Adding.readyEntity e adding |> Debug.log "Adding.readyEntity" of
                Nothing ->
                    -- try to add the egonet anyways
                    { model
                        | adding = adding
                    }
                        |> addEntityEgonet currency id isOutgoing neighbors.neighbors
                        |> n

                Just added ->
                    { model | adding = Adding.removeEntity e model.adding }
                        |> addEntity uc added

        BrowserGotEntityEgonetForAddress address currency id isOutgoing neighbors ->
            let
                e =
                    { currency = currency, address = address }

                adding =
                    (if isOutgoing then
                        Adding.setOutgoingForAddress

                     else
                        Adding.setIncomingForAddress
                    )
                        e
                        neighbors.neighbors
                        model.adding
            in
            case Adding.readyAddress e adding of
                Nothing ->
                    n { model | adding = adding }

                Just added ->
                    { model | adding = Adding.removeAddress e model.adding }
                        |> addAddress plugins uc added

        BrowserGotAddressNeighbors id isOutgoing neighbors ->
            ( model
            , neighbors.neighbors
                |> List.foldl
                    (\neighbor acc ->
                        Dict.update ( neighbor.address.currency, neighbor.address.entity )
                            (Maybe.map ((::) neighbor)
                                >> Maybe.withDefault [ neighbor ]
                                >> Just
                            )
                            acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map
                    (\( ( currency, entity ), neighbors_ ) ->
                        GetEntityEffect
                            { entity = entity
                            , currency = currency
                            , toMsg =
                                BrowserGotEntityForAddressNeighbor
                                    { anchor = id
                                    , isOutgoing = isOutgoing
                                    , neighbors = neighbors_
                                    }
                            }
                    )
            )

        BrowserGotAddressNeighborsTable id isOutgoing neighbors ->
            { model
                | browser = Browser.showAddressNeighbors id isOutgoing neighbors model.browser
            }
                |> n

        BrowserGotEntityForAddressNeighbor { anchor, isOutgoing, neighbors } entity ->
            Layer.getAddress anchor model.layers
                |> Maybe.andThen
                    (\address ->
                        Layer.getEntity address.entityId model.layers
                            |> Maybe.map (pair address)
                    )
                |> Maybe.map
                    (\( address, ent ) ->
                        handleAddressNeighbor plugins uc ( address, ent ) isOutgoing ( neighbors, entity ) model
                    )
                |> Maybe.withDefault (n model)
                |> mapSecond
                    ((++)
                        (getEntityEgonet
                            { currency = entity.currency
                            , entity = entity.entity
                            }
                            BrowserGotEntityEgonet
                            model.layers
                        )
                    )

        BrowserGotEntityNeighborsTable id isOutgoing neighbors ->
            { model
                | browser = Browser.showEntityNeighbors id isOutgoing neighbors model.browser
            }
                |> n

        BrowserGotEntityAddresses entityId addresses ->
            let
                added =
                    List.foldl
                        (\address acc ->
                            Layer.addAddressAtEntity
                                plugins
                                uc
                                acc.colors
                                entityId
                                address
                                acc.layers
                        )
                        { layers = model.layers
                        , new = Set.empty
                        , repositioned = Set.empty
                        , colors = model.config.colors
                        }
                        addresses.addresses
            in
            ( { model
                | layers =
                    added.layers
                , config =
                    model.config
                        |> s_colors added.colors
              }
                |> syncLinks added.repositioned
            , addresses.addresses
                |> List.map
                    (\address ->
                        GetAddressTagsEffect
                            { currency = address.currency
                            , address = address.address
                            , pagesize = 10
                            , nextpage = Nothing
                            , toMsg =
                                BrowserGotAddressTags
                                    { currency = address.currency
                                    , address = address.address
                                    }
                            }
                    )
            )

        BrowserGotEntityAddressesForTable id addresses ->
            { model
                | browser = Browser.showEntityAddresses id addresses model.browser
            }
                |> n

        BrowserGotAddressTags id tags ->
            { model
                | layers = Layer.updateAddresses id (Address.updateTags tags.addressTags) model.layers
            }
                |> n

        BrowserGotAddressTagsTable id tags ->
            { model
                | browser = Browser.showAddressTags id tags model.browser
            }
                |> n

        BrowserGotEntityAddressTagsTable id tags ->
            { model
                | browser = Browser.showEntityAddressTags id tags model.browser
            }
                |> n

        UserClickedAddressExpandHandle id isOutgoing ->
            case Layer.getAddress id model.layers of
                Nothing ->
                    n model

                Just address ->
                    if
                        (isOutgoing && address.address.outDegree <= maxExpandableNeighbors)
                            || (not isOutgoing && address.address.inDegree <= maxExpandableNeighbors)
                    then
                        ( model
                        , { currency = Id.currency id
                          , address = Id.addressId id
                          , isOutgoing = isOutgoing
                          , pagesize = 20
                          , includeLabels = False
                          , nextpage = Nothing
                          , toMsg = BrowserGotAddressNeighbors id isOutgoing
                          }
                            |> GetAddressNeighborsEffect
                            |> List.singleton
                        )

                    else
                        ( model
                        , Route.addressRoute
                            { currency = Id.currency id
                            , address = Id.addressId id
                            , table =
                                (if isOutgoing then
                                    Route.AddressOutgoingNeighborsTable

                                 else
                                    Route.AddressIncomingNeighborsTable
                                )
                                    |> Just
                            , layer = Id.layer id |> Just
                            }
                            |> NavPushRouteEffect
                            |> List.singleton
                        )

        BrowserGotNow time ->
            { model
                | browser =
                    model.browser
                        |> s_now time
            }
                |> n

        BrowserGotAddressTxs id data ->
            { model
                | browser = Browser.showAddressTxs id data model.browser
            }
                |> n

        BrowserGotEntityTxs id data ->
            { model
                | browser = Browser.showEntityTxs id data model.browser
            }
                |> n

        TableNewState state ->
            { model
                | browser = Browser.tableNewState state model.browser
            }
                |> n

        PluginMsg pid msgValue ->
            -- handled in src/Update.elm
            n model

        {- case context of
                 Plugin.Model ->
              let
                  ( new, outMsg, cmd ) =
                      Plugin.update pid plugins model.plugins msgValue (.graph >> .model)
              in
              ( List.foldl
                  (\( ctx, nw ) model ->
                      case ctx of
                          Plugin.Model ->
                              { model
                                  | plugins = new
                              }

                          Plugin.Address a ->
                              Layer.getAddress a model.layers
                                  |> Maybe.map
                                      (\address ->
                                          { model
                                              | layers = Layer.updateAddress address.id (\ad -> { ad | plugins = nw }) model.layers
                                          }
                                      )
                                  |> Maybe.withDefault model
                  )
                  new
                  |> updateByPluginOutMsg pid outMsg
              , List.map (PluginEffect context) cmd
              )

           Plugin.Address a ->
             Layer.getAddress a model.layers
                 |> Maybe.map
                     (\address ->
                         let
                             ( new, outMsg, cmd ) =
                                 Plugin.update pid plugins address.plugins msgValue (.graph >> .address)
                         in
                         ( { model
                             | layers = Layer.updateAddress address.id (\ad -> { ad | plugins = new }) model.layers
                           }
                             |> updateByPluginOutMsg pid outMsg
                         , List.map (PluginEffect context) cmd
                         )
                     )
                     |> Maybe.withDefault (n model)
        -}
        UserClickedContextMenu ->
            hideContextmenu model

        UserLeftContextMenu ->
            hideContextmenu model

        UserClickedAnnotateAddress id ->
            n model

        UserClickedRemoveAddress id ->
            n model

        UserClickedRemoveEntity id ->
            n model

        UserClickedAddressInEntityAddressesTable entityId address ->
            let
                added =
                    Layer.addAddressAtEntity
                        plugins
                        uc
                        model.config.colors
                        entityId
                        address
                        model.layers
            in
            ( { model
                | layers = added.layers
                , config = model.config |> s_colors added.colors
              }
                |> syncLinks added.repositioned
            , GetAddressTagsEffect
                { currency = address.currency
                , address = address.address
                , pagesize = 10
                , nextpage = Nothing
                , toMsg =
                    BrowserGotAddressTags
                        { currency = address.currency
                        , address = address.address
                        }
                }
                |> List.singleton
            )

        UserClickedAddressInNeighborsTable addressId isOutgoing neighbor ->
            let
                entityId =
                    Id.initEntityId
                        { currency = Id.currency addressId
                        , layer =
                            Id.layer addressId
                                + (if isOutgoing then
                                    1

                                   else
                                    -1
                                  )
                        , id = neighbor.address.entity
                        }

                added =
                    Layer.addAddressAtEntity plugins uc model.config.colors entityId neighbor.address model.layers
            in
            if Set.isEmpty added.new then
                ( model
                , [ GetEntityEffect
                        { entity = neighbor.address.entity
                        , currency = Id.currency addressId
                        , toMsg =
                            BrowserGotEntityForAddressNeighbor
                                { anchor = addressId
                                , isOutgoing = isOutgoing
                                , neighbors = [ neighbor ]
                                }
                        }
                  ]
                )

            else
                ( { model
                    | layers = added.layers
                    , config = model.config |> s_colors added.colors
                  }
                , [ GetAddressTagsEffect
                        { currency = Id.currency addressId
                        , address = Id.addressId addressId
                        , pagesize = 10
                        , nextpage = Nothing
                        , toMsg =
                            BrowserGotAddressTags
                                { currency = Id.currency addressId
                                , address = Id.addressId addressId
                                }
                        }
                  ]
                )

        UserClickedEntityInNeighborsTable entityId isOutgoing neighbor ->
            Layer.getEntity entityId model.layers
                |> Maybe.map
                    (\anchor ->
                        handleEntityNeighbors uc anchor isOutgoing [ neighbor ] model
                    )
                |> Maybe.withDefault (n model)

        NoOp ->
            n model


hideContextmenu : Model -> ( Model, List Effect )
hideContextmenu model =
    n { model | contextMenu = Nothing }


updateByRoute : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute plugins route model =
    case route of
        Route.Root ->
            deselect model
                |> n

        Route.Currency currency (Route.Address a table layer) ->
            layer
                |> Maybe.andThen
                    (\l -> Layer.getAddress (Id.initAddressId { currency = currency, id = a, layer = l }) model.layers)
                |> Maybe.Extra.orElseLazy
                    (\_ -> Layer.getFirstAddress { currency = currency, address = a } model.layers)
                |> Maybe.map
                    (\address ->
                        selectAddress address table model
                    )
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        let
                            browser =
                                Browser.loadingAddress { currency = currency, address = a } model.browser

                            ( browser2, effects ) =
                                table
                                    |> Maybe.map (\t -> Browser.showAddressTable t browser)
                                    |> Maybe.withDefault (n browser)
                        in
                        ( { model
                            | adding = Adding.loadAddress { currency = currency, address = a } model.adding
                            , browser = browser2
                          }
                        , [ GetEntityForAddressEffect
                                { address = a
                                , currency = currency
                                , toMsg = BrowserGotEntityForAddress a
                                }
                          , GetAddressEffect
                                { address = a
                                , currency = currency
                                , toMsg = BrowserGotAddress
                                }
                          ]
                            ++ effects
                        )
                    )

        Route.Currency currency (Route.Entity e table layer) ->
            layer
                |> Maybe.andThen
                    (\l -> Layer.getEntity (Id.initEntityId { currency = currency, id = e, layer = l }) model.layers)
                |> Maybe.Extra.orElseLazy
                    (\_ -> Layer.getFirstEntity { currency = currency, entity = e } model.layers)
                |> Maybe.map
                    (\entity ->
                        selectEntity entity table model
                    )
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        let
                            browser =
                                Browser.loadingEntity { currency = currency, entity = e } model.browser

                            ( browser2, effects ) =
                                table
                                    |> Maybe.map (\t -> Browser.showEntityTable t browser)
                                    |> Maybe.withDefault (n browser)
                        in
                        ( { model
                            | browser = browser2
                            , adding = Adding.loadEntity { currency = currency, entity = e } model.adding
                          }
                        , [ GetEntityEffect
                                { entity = e
                                , currency = currency
                                , toMsg = BrowserGotEntity
                                }
                          ]
                            ++ getEntityEgonet
                                { currency = currency
                                , entity = e
                                }
                                BrowserGotEntityEgonet
                                model.layers
                            ++ effects
                        )
                    )

        Route.Currency currency (Route.Tx t) ->
            n model

        Route.Currency currency (Route.Block b) ->
            n model

        Route.Label l ->
            n model

        Route.Plugin ( pid, value ) ->
            n model


updateSize : Int -> Int -> Model -> Model
updateSize w h model =
    { model
        | size =
            model.size
                |> Maybe.map
                    (\{ x, y } ->
                        { x = x + toFloat w
                        , y = y + toFloat h
                        }
                    )
    }


addAddressNeighborsWithEntity : Plugins -> Update.Config -> ( Address, Entity ) -> Bool -> ( List Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> ( Model, List Address, Set EntityId )
addAddressNeighborsWithEntity plugins uc ( anchorAddress, anchorEntity ) isOutgoing ( neighbors, entity ) model =
    let
        acc =
            Layer.addEntityNeighbors uc anchorEntity isOutgoing model.config.colors [ entity ] model.layers

        _ =
            Debug.log "syncLinks.addEntityNeighbors.repos" acc.repositioned
    in
    Set.toList acc.new
        |> List.head
        |> Debug.log "syncLinks.addAddressNeighborsWithEntity.newEntity"
        |> Maybe.map
            (\new ->
                let
                    _ =
                        Layer.getEntity new acc.layers
                            |> Maybe.map Model.Graph.Entity.getY
                            |> Debug.log "syncLinks.newEntity.y"

                    added =
                        neighbors
                            |> List.foldl
                                (\neighbor added_ ->
                                    let
                                        added__ =
                                            Layer.addAddressAtEntity plugins uc model.config.colors new neighbor.address added_.layers
                                    in
                                    { layers = added__.layers
                                    , new = Set.union added__.new added_.new
                                    , repositioned = Set.union added_.repositioned added__.repositioned
                                    , colors = Dict.union added__.colors added_.colors
                                    }
                                )
                                { layers = acc.layers
                                , colors = acc.colors
                                , new = Set.empty
                                , repositioned = acc.repositioned
                                }
                in
                ( { model
                    | layers = added.layers
                    , config =
                        model.config
                            |> s_colors added.colors
                  }
                , Set.toList added.new
                    |> List.filterMap
                        (\a -> Layer.getAddress a added.layers)
                , added.repositioned
                )
            )
        |> Maybe.withDefault
            ( model
            , []
            , acc.repositioned
            )


addEntityNeighbors : Update.Config -> Entity -> Bool -> List Api.Data.NeighborEntity -> Model -> ( Model, List ( Api.Data.NeighborEntity, Entity ), Set EntityId )
addEntityNeighbors uc anchor isOutgoing neighbors model =
    let
        acc =
            Layer.addEntityNeighbors uc anchor isOutgoing model.config.colors (List.map .entity neighbors) model.layers

        aligned =
            neighbors
                |> List.filterMap
                    (\neighbor ->
                        let
                            entityId =
                                Id.initEntityId
                                    { currency = anchor.entity.currency
                                    , layer =
                                        Id.layer anchor.id
                                            + (if isOutgoing then
                                                1

                                               else
                                                -1
                                              )
                                    , id = neighbor.entity.entity
                                    }
                        in
                        if Set.member entityId acc.new then
                            Layer.getEntity entityId acc.layers
                                |> Maybe.map (pair neighbor)

                        else
                            Nothing
                    )
    in
    ( { model
        | layers = acc.layers
        , config =
            model.config
                |> s_colors acc.colors
      }
    , aligned
    , acc.repositioned
    )


addEntityLinks : Entity -> Bool -> List ( Api.Data.NeighborEntity, Entity ) -> Model -> Model
addEntityLinks anchor isOutgoing neighbors model =
    let
        linkData =
            neighbors
                |> List.map (mapFirst Link.fromNeighbor)

        layers =
            if isOutgoing then
                Layer.updateEntityLinks { currency = Id.currency anchor.id, entity = Id.entityId anchor.id } linkData model.layers

            else
                linkData
                    |> List.foldl
                        (\( linkDatum, neighbor ) ->
                            Layer.updateEntityLinks
                                { currency = Id.currency neighbor.id, entity = Id.entityId neighbor.id }
                                [ ( linkDatum, anchor ) ]
                        )
                        model.layers
    in
    { model
        | layers = layers
    }


addAddressLink : Address -> Bool -> ( Api.Data.NeighborAddress, Address ) -> Model -> Model
addAddressLink anchor isOutgoing ( neighbor, target ) model =
    let
        linkData =
            Link.fromNeighbor neighbor

        layers =
            if isOutgoing then
                Layer.updateAddressLink { currency = Id.currency anchor.id, address = Id.addressId anchor.id } ( linkData, target ) model.layers

            else
                Layer.updateAddressLink
                    { currency = Id.currency target.id, address = Id.addressId target.id }
                    ( linkData, anchor )
                    model.layers
    in
    { model
        | layers = layers
    }


addEntityEgonet : String -> Int -> Bool -> List Api.Data.NeighborEntity -> Model -> Model
addEntityEgonet currency entity isOutgoing neighbors model =
    let
        entities =
            List.map
                (\neighbor ->
                    Layer.getEntities neighbor.entity.currency neighbor.entity.entity model.layers
                        |> List.map (pair neighbor)
                )
                neighbors
                |> List.concat

        anchors =
            Layer.getEntities currency entity model.layers

        _ =
            anchors
                |> List.map .id
                |> Debug.log "addEntityEgonet.anchors"
    in
    List.foldl
        (\anchor model_ ->
            addEntityLinks anchor isOutgoing entities model_
        )
        model
        anchors


handleEntityNeighbors : Update.Config -> Entity -> Bool -> List Api.Data.NeighborEntity -> Model -> ( Model, List Effect )
handleEntityNeighbors uc anchor isOutgoing neighbors model =
    let
        ( newModel, new, repositioned ) =
            addEntityNeighbors uc anchor isOutgoing neighbors model
    in
    ( addEntityLinks anchor isOutgoing new newModel
        |> syncLinks repositioned
    , neighbors
        |> List.map
            (\{ entity } ->
                getEntityEgonet
                    { currency = entity.currency
                    , entity = entity.entity
                    }
                    BrowserGotEntityEgonet
                    newModel.layers
            )
        |> List.concat
    )


{-|

    neighbors contains a list of address neighbors and their parent entity.

-}
handleAddressNeighbor : Plugins -> Update.Config -> ( Address, Entity ) -> Bool -> ( List Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> ( Model, List Effect )
handleAddressNeighbor plugins uc anchor isOutgoing neighbors model =
    let
        ( newModel, new, repositionedEntities ) =
            addAddressNeighborsWithEntity plugins uc anchor isOutgoing neighbors model
    in
    ( new
        |> List.foldl
            (\address model_ ->
                first neighbors
                    |> List.Extra.find (\n -> n.address.currency == address.address.currency && n.address.address == address.address.address)
                    |> Maybe.map
                        (\neighbor ->
                            addAddressLink (first anchor) isOutgoing ( neighbor, address ) model_
                        )
                    |> Maybe.withDefault model_
            )
            newModel
        |> syncLinks repositionedEntities
    , first neighbors
        |> List.map
            (\neighbor ->
                GetAddressTagsEffect
                    { currency = neighbor.address.currency
                    , address = neighbor.address.address
                    , pagesize = 10
                    , nextpage = Nothing
                    , toMsg =
                        BrowserGotAddressTags
                            { currency = neighbor.address.currency
                            , address = neighbor.address.address
                            }
                    }
            )
    )


syncLinks : Set EntityId -> Model -> Model
syncLinks repositioned model =
    { model
        | layers = Layer.syncLinks repositioned model.layers
    }


selectAddress : Address -> Maybe Route.AddressTable -> Model -> ( Model, List Effect )
selectAddress address table model =
    let
        browser =
            Browser.showAddress address model.browser

        ( browser2, effects ) =
            table
                |> Maybe.map (\t -> Browser.showAddressTable t browser)
                |> Maybe.withDefault (n browser)
    in
    ( { model
        | browser = browser2
        , selected = SelectedAddress address.id
      }
    , effects
    )


selectEntity : Entity -> Maybe Route.EntityTable -> Model -> ( Model, List Effect )
selectEntity entity table model =
    let
        browser =
            Browser.showEntity entity model.browser

        ( browser2, effects ) =
            table
                |> Maybe.map (\t -> Browser.showEntityTable t browser)
                |> Maybe.withDefault (n browser)
    in
    ( { model
        | browser = browser2
        , selected = SelectedEntity entity.id
      }
    , effects
    )


deselect : Model -> Model
deselect model =
    { model
        | selected = SelectedNone
        , browser =
            model.browser
                |> s_visible False
    }


draggingToClick : Coords -> Coords -> Bool
draggingToClick start current =
    Coords.betrag start current < 2
