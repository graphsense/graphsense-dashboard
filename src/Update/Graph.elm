module Update.Graph exposing (At(..), More(..), SearchResult, addAddress, addAddressLinks, addAddressNeighborsWithEntity, addAddressesAtEntity, addEntity, addEntityEgonet, addEntityLinks, addEntityNeighbors, addUserTag, checkTagsCanBeApplied, cleanHistory, decodeYamlTag, deleteUserTag, deselect, deselectHighlighter, deselectLayers, deserialize, deserializeByVersion, draggingToClick, extendTransformWithBoundingBox, forcePushHistory, fromDeserialized, getToolElement, handleAddressNeighbor, handleEntityNeighbors, handleEntitySearchResult, handleNotFound, hideContextmenu, importTagPack, insertAddressShadowLinks, insertEntityShadowLinks, layerDelta, loadAddress, loadAddressPath, loadEntity, loadEntityPath, loadNextAddress, loadNextEntity, makeHistoryEntry, makeLegend, makeTagPack, normalizeDeserializedEntityTag, prepareSearchResult, pushHistory, refreshBrowserAddress, refreshBrowserEntity, refreshBrowserEntityIf, repositionHovercardCmd, repositionHovercards, selectAddress, selectAddressLink, selectAddressLinkIfLoaded, selectEntity, selectEntityLink, selectEntityLinkIfLoaded, storeUserTag, syncBrowser, syncLinks, syncSelection, tagId, tagInputToUserTag, toolElementResultToTool, toolVisible, undoRedo, update, updateAddresses, updateByMsg, updateByPluginOutMsg, updateByRoute, updateByRoute_, updateEntitiesIf, updateLegend, updateSearch, updateTransformByBoundingBox)

import Api.Data
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Components.Table as Table
import Config.Graph exposing (maxExpandableAddresses, maxExpandableNeighbors)
import Config.Update as Update
import DateFormat
import Decode.Graph044 as Graph044
import Decode.Graph045 as Graph045
import Decode.Graph050 as Graph050
import Decode.Graph100 as Graph100
import Dict exposing (Dict)
import Effect.Api exposing (Effect(..), getAddressEgonet, getEntityEgonet)
import Effect.Graph exposing (Effect(..))
import File
import File.Select
import Hovercard
import Init.Graph.ContextMenu as ContextMenu
import Init.Graph.Highlighter as Highlighter
import Init.Graph.History as History
import Init.Graph.Id as Id
import Init.Graph.Search as Search
import Init.Graph.Tag as Tag
import IntDict exposing (IntDict)
import Json.Decode
import Json.Encode
import List.Extra
import Log
import Maybe.Extra
import Model.Address as A
import Model.Entity as E
import Model.Graph exposing (..)
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Browser as Browser
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Deserialize exposing (..)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Highlighter as Highlighter
import Model.Graph.History as History
import Model.Graph.History.Entry as Entry
import Model.Graph.Id as Id exposing (AddressId, EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Link as Link exposing (Link)
import Model.Graph.Search as Search
import Model.Graph.Tag as Tag
import Model.Graph.Tool as Tool
import Model.Graph.Transform as Transform
import Model.Loadable exposing (Loadable(..))
import Model.Node as Node
import Model.Search
import Msg.Graph as Msg exposing (Msg(..))
import Plugin.Msg as Plugin
import Plugin.Update as Plugin exposing (Plugins)
import PluginInterface.Msg as PluginInterface
import Ports
import RecordSetter exposing (..)
import Route.Graph as Route
import Set exposing (Set)
import Task
import Time
import Tuple exposing (..)
import Update.Graph.Adding as Adding exposing (normalizeEth)
import Update.Graph.Address as Address
import Update.Graph.Browser as Browser
import Update.Graph.Coords as Coords
import Update.Graph.Entity as Entity
import Update.Graph.Highlighter as Highlighter
import Update.Graph.History as History
import Update.Graph.Layer as Layer
import Update.Graph.Search as Search
import Update.Graph.Tag as Tag
import Update.Graph.Transform as Transform
import Util exposing (n)
import Util.Data as Data
import Util.Graph
import Util.Graph.History as History
import Yaml.Decode
import Yaml.Encode


addAddress :
    Plugins
    -> Update.Config
    ->
        { address : Api.Data.Address
        , entity : Api.Data.Entity
        , incoming : List Api.Data.NeighborEntity
        , outgoing : List Api.Data.NeighborEntity
        , anchor : Maybe ( Bool, Id.AddressId )
        }
    -> Model
    -> ( Model, List Effect )
addAddress plugins uc { address, entity, incoming, outgoing, anchor } model =
    let
        entityAnchor =
            anchor
                |> Maybe.andThen
                    (\( io, ad ) ->
                        Layer.getAddress ad model.layers
                            |> Maybe.map (.entityId >> pair io)
                    )

        ( newModel, eff ) =
            addEntity plugins
                uc
                { entity = entity
                , incoming = incoming
                , outgoing = outgoing
                }
                entityAnchor
                model

        added =
            -- grab the added entities ...
            eff
                |> List.foldl
                    (\ef entityIds ->
                        case ef of
                            InternalGraphAddedEntitiesEffect ids ->
                                Set.union entityIds ids

                            _ ->
                                entityIds
                    )
                    Set.empty
                -- ... and add the address to them
                |> Set.foldl
                    (\entityId added_ ->
                        Layer.addAddressAtEntity plugins
                            uc
                            entityId
                            address
                            added_
                    )
                    { layers = newModel.layers
                    , new = Set.empty
                    , repositioned = Set.empty
                    }

        newModel_ =
            { newModel
                | layers =
                    added.layers
                        |> addUserTag added.new model.userAddressTags
            }
                |> syncLinks added.repositioned

        addedAddress =
            added.new
                |> Set.toList
                |> List.head
                |> Maybe.andThen (\a -> Layer.getAddress a newModel_.layers)

        getTagsEffect =
            BrowserGotAddressTags
                { currency = address.currency
                , address = address.address
                }
                |> GetAddressTagsEffect
                    { address = address.address
                    , currency = address.currency
                    , nextpage = Nothing
                    , pagesize = 10
                    , includeBestClusterTag = False
                    }
                |> ApiEffect

        tbl =
            case newModel.route of
                Route.Currency _ (Route.Address _ t _) ->
                    t

                _ ->
                    Nothing
    in
    addedAddress
        |> Maybe.map
            (\a ->
                selectAddress a tbl newModel_
                    |> mapSecond
                        ((++)
                            (getAddressEgonet a.id BrowserGotAddressEgonet newModel_.layers
                                |> List.map ApiEffect
                            )
                        )
            )
        |> Maybe.withDefault (n newModel_)
        |> mapSecond ((++) eff)
        |> mapSecond ((::) getTagsEffect)
        |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))


addEntity : Plugins -> Update.Config -> { entity : Api.Data.Entity, incoming : List Api.Data.NeighborEntity, outgoing : List Api.Data.NeighborEntity } -> Maybe ( Bool, Id.EntityId ) -> Model -> ( Model, List Effect )
addEntity plugins uc { entity, incoming, outgoing } anchor model =
    anchor
        |> Maybe.andThen
            (\( isOutgoing, anch ) ->
                Layer.getEntity anch model.layers
                    |> Maybe.map
                        (\e ->
                            if isOutgoing then
                                ( [ e ], [] )

                            else
                                ( [], [ e ] )
                        )
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                let
                    findEntities e =
                        (++)
                            (Layer.getEntities e.entity.currency e.entity.entity model.layers)

                    filterSelf : Api.Data.NeighborEntity -> Bool
                    filterSelf neighbor =
                        neighbor.entity.entity /= entity.entity

                    outgoingAnchors =
                        incoming
                            |> List.filter filterSelf
                            |> List.foldl findEntities []

                    incomingAnchors =
                        outgoing
                            |> List.filter filterSelf
                            |> List.foldl findEntities []
                in
                ( outgoingAnchors, incomingAnchors )
            )
        |> (\( oa, ia ) ->
                let
                    outgoingAnchors =
                        oa
                            |> List.map (\e -> ( Id.layer e.id, ( e, True ) ))
                            |> IntDict.fromList

                    incomingAnchors =
                        ia
                            |> List.map (\e -> ( Id.layer e.id, ( e, False ) ))
                            |> IntDict.fromList

                    added =
                        if IntDict.isEmpty outgoingAnchors && IntDict.isEmpty incomingAnchors then
                            Layer.addEntity plugins uc entity model.layers

                        else
                            Layer.addEntitiesAt plugins
                                uc
                                (Layer.anchorsToPositions (Just outgoingAnchors) model.layers)
                                [ entity ]
                                { layers = model.layers
                                , new = Set.empty
                                , repositioned = Set.empty
                                }
                                |> Layer.addEntitiesAt plugins
                                    uc
                                    (Layer.anchorsToPositions (Just incomingAnchors) model.layers)
                                    [ entity ]

                    newModel =
                        { model
                            | layers = added.layers
                        }
                            |> syncLinks added.repositioned
                            |> addEntityEgonet entity.currency entity.entity True outgoing
                            |> addEntityEgonet entity.currency entity.entity False incoming
                in
                added.new
                    |> Set.toList
                    |> List.head
                    |> Maybe.andThen (\e -> Layer.getEntity e added.layers)
                    |> Maybe.map (\e -> selectEntity e Nothing newModel)
                    |> Maybe.map (mapSecond ((::) (InternalGraphAddedEntitiesEffect added.new)))
                    |> Maybe.withDefault (n newModel)
           )


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory msg
        |> updateByMsg plugins uc msg
        |> mapFirst (syncBrowser model)


syncBrowser : Model -> Model -> Model
syncBrowser old model =
    { model
        | browser =
            if old.layers == model.layers then
                model.browser

            else
                model.browser
                    |> s_layers model.layers
    }


loadNextAddress : Plugins -> Update.Config -> Model -> Id.AddressId -> ( Model, List Effect )
loadNextAddress plugins _ model id =
    Adding.getNextAddressFor id model.adding
        |> Maybe.map
            (\nextId ->
                { model
                    | adding = Adding.popAddressPath model.adding
                }
                    |> loadAddress
                        plugins
                        { currency = Id.currency nextId
                        , address = Id.addressId nextId
                        , table = Nothing
                        , at = AtAnchor True id |> Just
                        }
                    |> (\( m, eff ) -> ( m, eff ))
            )
        |> Maybe.withDefault (n model)


loadNextEntity : Plugins -> Update.Config -> Model -> Id.EntityId -> ( Model, List Effect )
loadNextEntity plugins _ model id =
    Adding.getNextEntityFor id model.adding
        |> Maybe.map
            (\nextId ->
                { model
                    | adding = Adding.popEntityPath model.adding
                }
                    |> loadEntity
                        plugins
                        { currency = Id.currency nextId
                        , entity = Id.entityId nextId
                        , table = Nothing
                        , layer = Id.layer nextId |> Just
                        }
                    |> (\( m, eff ) -> ( m, eff ))
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                selectEntityLinkIfLoaded model
            )


updateByMsg : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
updateByMsg plugins uc msg model =
    case Log.truncate "msg" msg of
        UserScrolledTable pos ->
            let
                ( browser, effects ) =
                    Browser.infiniteScroll pos model.browser
            in
            effects
                |> pair { model | browser = browser }

        InternalGraphAddedAddresses ids ->
            Set.toList ids
                |> List.head
                |> Maybe.map (loadNextAddress plugins uc model)
                |> Maybe.withDefault (n model)
                |> mapFirst (updateLegend uc)

        InternalGraphAddedEntities ids ->
            let
                ( transform, eff ) =
                    Transform.delay ids model.transform

                ( model2, pathEff ) =
                    Set.toList ids
                        |> List.head
                        |> Maybe.map (loadNextEntity plugins uc model)
                        |> Maybe.withDefault (n model)
            in
            ( model2
                |> s_transform transform
            , CmdEffect eff
                :: pathEff
            )
                |> mapFirst (updateLegend uc)

        RuntimeDebouncedAddingEntities ->
            let
                ( newTransform, isSteady ) =
                    Transform.pop model.transform

                newModel =
                    { model
                        | transform = newTransform
                    }
            in
            if isSteady then
                model.transform.collectingAddedEntityIds
                    |> Set.toList
                    |> List.filterMap (\id -> Layer.getEntity id model.layers)
                    |> Layer.getBoundingBoxOfEntities
                    |> Maybe.map (extendTransformWithBoundingBox uc newModel)
                    |> Maybe.withDefault newModel
                    |> n

            else
                n newModel

        InternalGraphSelectedAddress _ ->
            n model

        -- handled upstream
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
                ( case ( model.activeTool.toolbox, model.activeTool.element ) of
                    ( Tool.Highlighter, Just ( el, vis ) ) ->
                        toolVisible
                            (if vis then
                                deselectHighlighter model

                             else
                                model
                            )
                            el
                            vis

                    _ ->
                        model
                , Route.rootRoute
                    |> NavPushRouteEffect
                    |> List.singleton
                )

            else
                n model

        UserWheeledOnGraph x y z ->
            uc.size
                |> Maybe.map
                    (\size ->
                        { model
                            | transform =
                                Transform.wheel
                                    { width = size.width
                                    , height = size.height
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
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            Dragging model.transform coords coords

                        _ ->
                            model.dragging
            }
                |> n

        UserPushesLeftMouseButtonOnEntity id coords ->
            { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            DraggingNode id coords coords

                        _ ->
                            model.dragging
            }
                |> n

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging transform start _ ->
                    { model
                        | transform = Transform.update start coords transform
                        , dragging = Dragging transform start coords
                    }
                        |> repositionHovercards

                DraggingNode id start _ ->
                    let
                        vector =
                            Transform.vector start coords model.transform
                    in
                    { model
                        | layers =
                            Layer.moveEntity id vector model.layers
                        , dragging = DraggingNode id start coords
                    }
                        |> syncLinks (Set.singleton id)
                        |> repositionHovercards

        UserReleasesMouseButton ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging _ _ _ ->
                    { model
                        | dragging = NoDragging
                    }
                        |> repositionHovercards

                DraggingNode id _ _ ->
                    { model
                        | layers = Layer.releaseEntity id model.layers
                        , dragging = NoDragging
                    }
                        |> repositionHovercards

        UserPressesEscape ->
            deselectHighlighter model |> n

        UserClickedAddress id ->
            case Highlighter.getSelectedColor model.highlights of
                Nothing ->
                    Route.addressRoute
                        { currency = Id.currency id
                        , address = Id.addressId id
                        , table = Route.getAddressTable model.route
                        , layer = Id.layer id |> Just
                        }
                        |> NavPushRouteEffect
                        |> List.singleton
                        |> pair model

                Just color ->
                    { model
                        | layers =
                            Layer.updateAddress id (Address.updateColor color) model.layers
                                |> Layer.syncLinks
                                    (Layer.getAddress id model.layers
                                        |> Maybe.map .entityId
                                        |> Maybe.map List.singleton
                                        |> Maybe.withDefault []
                                    )
                    }
                        |> n

        UserRightClickedAddress id coords ->
            Layer.getAddress id model.layers
                |> Maybe.map
                    (\address ->
                        { model
                            | contextMenu =
                                ContextMenu.initAddress (Coords.relativeToGraph uc.size coords) address
                                    |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserClickedAddressActions id coords ->
            Layer.getAddress id model.layers
                |> Maybe.map
                    (\address ->
                        { model
                            | contextMenu =
                                ContextMenu.initAddress (Coords.relativeToGraph uc.size coords) address
                                    |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserHoversAddress id ->
            n { model | hovered = HoveredAddress id }

        UserClickedEntity id moved ->
            if draggingToClick { x = 0, y = 0 } moved then
                case Highlighter.getSelectedColor model.highlights of
                    Nothing ->
                        ( model
                        , Route.entityRoute
                            { currency = Id.currency id
                            , entity = Id.entityId id
                            , table = Route.getEntityTable model.route
                            , layer = Id.layer id |> Just
                            }
                            |> NavPushRouteEffect
                            |> List.singleton
                        )

                    Just color ->
                        { model
                            | layers =
                                Layer.updateEntity id (Entity.updateColor color) model.layers
                                    |> Layer.syncLinks [ id ]
                        }
                            |> n

            else
                n model

        UserRightClickedEntity id coords ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\entity ->
                        { model
                            | contextMenu =
                                ContextMenu.initEntity (Coords.relativeToGraph uc.size coords) entity
                                    |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserClickedEntityActions id coords ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\entity ->
                        { model
                            | contextMenu =
                                ContextMenu.initEntity (Coords.relativeToGraph uc.size coords) entity
                                    |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserClickedTransactionActions hash currency coords ->
            { model
                | contextMenu =
                    ContextMenu.initTransaction (Coords.relativeToGraph uc.size coords) hash currency
                        |> Just
            }
                |> n

        UserHoversEntity id ->
            n { model | hovered = HoveredEntity id }

        UserHoversEntityLink id ->
            { model
                | hovered = HoveredEntityLink id
            }
                |> n

        UserClicksEntityLink id ->
            ( model
            , Route.entitylinkRoute
                { currency = first id |> Id.currency
                , src = first id |> Id.entityId
                , srcLayer = first id |> Id.layer
                , dst = second id |> Id.entityId
                , dstLayer = second id |> Id.layer
                , table = Route.getAddresslinkTable model.route
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserRightClicksEntityLink id coords ->
            { model
                | contextMenu =
                    ContextMenu.initEntityLink (Coords.relativeToGraph uc.size coords) id
                        |> Just
            }
                |> n

        UserHoversAddressLink id ->
            { model
                | hovered = HoveredAddressLink id
            }
                |> n

        UserClicksAddressLink id ->
            ( model
            , Route.addresslinkRoute
                { currency = first id |> Id.currency
                , src = first id |> Id.addressId
                , srcLayer = first id |> Id.layer
                , dst = second id |> Id.addressId
                , dstLayer = second id |> Id.layer
                , table = Route.getAddresslinkTable model.route
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserRightClicksAddressLink id coords ->
            { model
                | contextMenu =
                    ContextMenu.initAddressLink (Coords.relativeToGraph uc.size coords) id
                        |> Just
            }
                |> n

        UserLeavesThing ->
            { model
                | hovered = HoveredNone
            }
                |> n

        UserClickedAddressesExpand id ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\entity ->
                        if entity.entity.noAddresses < maxExpandableAddresses then
                            if Dict.size entity.addresses == entity.entity.noAddresses then
                                { model
                                    | layers =
                                        entity.addresses
                                            |> Dict.foldl
                                                (\i _ layers ->
                                                    Layer.removeAddress i layers
                                                )
                                                model.layers
                                }
                                    |> n

                            else
                                ( model
                                , [ BrowserGotEntityAddresses id
                                        |> GetEntityAddressesEffect
                                            { currency = Id.currency id
                                            , entity = Id.entityId id
                                            , pagesize = maxExpandableAddresses
                                            , nextpage = Nothing
                                            }
                                        |> ApiEffect
                                  ]
                                )

                        else
                            ( model
                            , [ Route.entityRoute
                                    { currency = Id.currency id
                                    , entity = Id.entityId id
                                    , table = Just Route.EntityAddressesTable
                                    , layer = Id.layer id |> Just
                                    }
                                    |> NavPushRouteEffect
                              ]
                            )
                    )
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
                        , BrowserGotEntityNeighbors id isOutgoing
                            |> GetEntityNeighborsEffect
                                { currency = Id.currency id
                                , entity = Id.entityId id
                                , isOutgoing = isOutgoing
                                , onlyIds = Nothing
                                , pagesize = 20
                                , includeLabels = False
                                , nextpage = Nothing
                                }
                            |> ApiEffect
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

        BrowserGotActor actor ->
            let
                ( newbrowser, effects ) =
                    Browser.showActor actor model.browser
            in
            ( { model
                | browser = newbrowser
              }
            , effects
            )

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
                        |> List.map ApiEffect
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
                        |> addEntity plugins uc added Nothing

        BrowserGotEntityNeighbors id isOutgoing neighbors ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\anchor ->
                        handleEntityNeighbors plugins uc anchor isOutgoing neighbors.neighbors model
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
            case Adding.readyEntity e adding of
                Nothing ->
                    -- try to add the egonet anyways
                    { model
                        | adding = adding
                    }
                        |> addEntityEgonet currency id isOutgoing neighbors.neighbors
                        |> n

                Just added ->
                    { model | adding = Adding.removeEntity e model.adding }
                        |> addEntity plugins uc added Nothing

        BrowserGotAddressEgonet anchor isOutgoing neighbors ->
            Layer.getAddresses (A.fromId anchor) model.layers
                |> List.foldl
                    (\anch mo ->
                        neighbors.neighbors
                            |> List.filterMap
                                (\n ->
                                    let
                                        id =
                                            Id.initAddressId
                                                { layer =
                                                    Id.layer anch.id
                                                        |> layerDelta isOutgoing
                                                , id = n.address.address
                                                , currency = n.address.currency
                                                }
                                    in
                                    Layer.getAddress id model.layers
                                        |> Maybe.map (pair n)
                                )
                            |> (\neighs -> addAddressLinks anch isOutgoing neighs mo)
                    )
                    model
                |> selectAddressLinkIfLoaded

        BrowserGotEntityEgonetForAddress address currency _ isOutgoing neighbors ->
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
                |> List.filter
                    (\n -> Util.Graph.filterTxValue model.config n.address.currency n.value n.tokenValues)
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
                        BrowserGotEntityForAddressNeighbor
                            { anchor = id
                            , isOutgoing = isOutgoing
                            , neighbors = neighbors_
                            }
                            |> GetEntityEffect
                                { entity = entity
                                , currency = currency
                                }
                            |> ApiEffect
                    )
            )

        BrowserGotAddressNeighborsTable id isOutgoing neighbors ->
            { model
                | browser = Browser.showAddressNeighbors model.config id isOutgoing neighbors model.browser
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
                            |> List.map ApiEffect
                        )
                    )

        BrowserGotEntityNeighborsTable id isOutgoing neighbors ->
            { model
                | browser = Browser.showEntityNeighbors model.config id isOutgoing neighbors model.browser
            }
                |> n

        BrowserGotEntityAddresses entityId addresses ->
            addAddressesAtEntity plugins uc entityId addresses.addresses model

        BrowserGotEntityAddressesForTable id addresses ->
            { model
                | browser = Browser.showEntityAddresses id addresses model.browser
            }
                |> n

        BrowserGotAddressTags id tags ->
            model
                |> updateAddresses id (Address.updateTags tags.addressTags)
                |> n

        BrowserGotActorTagsTable actor tags ->
            { model
                | browser = Browser.showActorTags actor.actorId tags model.browser
            }
                |> n

        BrowserGotLabelAddressTags label tags ->
            { model
                | browser = Browser.showLabelAddressTags label tags model.browser
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
                        , BrowserGotAddressNeighbors id isOutgoing
                            |> GetAddressNeighborsEffect
                                { currency = Id.currency id
                                , address = Id.addressId id
                                , isOutgoing = isOutgoing
                                , pagesize = 20
                                , includeLabels = False
                                , includeActors = True
                                , onlyIds = Nothing
                                , nextpage = Nothing
                                }
                            |> ApiEffect
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
                | browser =
                    if Data.isAccountLike id.currency then
                        Browser.showAddressTxsAccount model.config id data model.browser

                    else
                        Browser.showAddressTxsUtxo id data model.browser
            }
                |> n

        BrowserGotAddresslinkTxs id data ->
            { model
                | browser =
                    if Data.isAccountLike id.currency then
                        Browser.showAddresslinkTxsAccount model.config id data model.browser

                    else
                        Browser.showAddresslinkTxsUtxo id data model.browser
            }
                |> n

        BrowserGotEntityTxs id data ->
            { model
                | browser =
                    if Data.isAccountLike id.currency then
                        Browser.showEntityTxsAccount model.config id data model.browser

                    else
                        Browser.showEntityTxsUtxo id data model.browser
            }
                |> n

        BrowserGotEntitylinkTxs id data ->
            { model
                | browser =
                    if Data.isAccountLike id.currency then
                        Browser.showEntitylinkTxsAccount model.config id data model.browser

                    else
                        Browser.showEntitylinkTxsUtxo id data model.browser
            }
                |> n

        BrowserGotTx accountCurrency data ->
            let
                ( browser, cmd ) =
                    Browser.showTx data accountCurrency model.browser
            in
            ( { model
                | browser = browser
              }
            , cmd
            )

        BrowserGotTxUtxoAddresses id isOutgoing data ->
            { model
                | browser = Browser.showTxUtxoAddresses id isOutgoing data model.browser
            }
                |> n

        BrowserGotBlock data ->
            Browser.showBlock data model.browser
                |> mapFirst
                    (\browser ->
                        { model
                            | browser = browser
                        }
                    )

        BrowserGotBlockTxs id data ->
            { model
                | browser =
                    if Data.isAccountLike id.currency then
                        Browser.showBlockTxsAccount model.config id data model.browser

                    else
                        Browser.showBlockTxsUtxo id data model.browser
            }
                |> n

        BrowserGotTokenTxs id data ->
            { model
                | browser =
                    Browser.showTokenTxs model.config id data model.browser
            }
                |> n

        TableNewState state ->
            { model
                | browser = Browser.tableNewState state model.browser
            }
                |> n

        PluginMsg _ ->
            -- handled in src/Update.elm
            n model

        UserClickedContextMenu ->
            hideContextmenu model

        UserLeftContextMenu ->
            hideContextmenu model

        UserClickedAnnotateAddress id ->
            let
                ( tag, cmd ) =
                    model.layers
                        |> Layer.getAddress id
                        |> Maybe.andThen .userTag
                        |> Tag.initAddressTag id
            in
            ( { model
                | tag =
                    tag
                        |> Just
              }
            , CmdEffect cmd |> List.singleton
            )

        UserClickedAnnotateEntity id ->
            let
                ( tag, cmd ) =
                    model.layers
                        |> Layer.getEntity id
                        |> Maybe.andThen .userTag
                        |> Tag.initEntityTag id
            in
            ( { model
                | tag =
                    tag
                        |> Just
              }
            , CmdEffect cmd |> List.singleton
            )

        UserInputsTagSource input ->
            model.tag
                |> Maybe.map
                    (\tag ->
                        { model
                            | tag = Tag.inputSource input tag |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserInputsTagCategory input ->
            model.tag
                |> Maybe.map
                    (\tag ->
                        { model
                            | tag = Tag.inputCategory input tag |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserInputsTagAbuse input ->
            model.tag
                |> Maybe.map
                    (\tag ->
                        { model
                            | tag = Tag.inputAbuse input tag |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserClicksCloseTagHovercard ->
            { model
                | tag = Nothing
            }
                |> n

        UserClicksDeleteTag ->
            model.tag
                |> Maybe.andThen .existing
                |> Maybe.map (\tag -> deleteUserTag uc tag model)
                |> Maybe.withDefault model
                |> n

        UserSubmitsTagInput ->
            model.tag
                |> Maybe.andThen
                    (.input >> tagInputToUserTag model)
                |> Maybe.map (\tag -> storeUserTag uc tag model)
                |> Maybe.withDefault model
                |> n

        UserClickedUserTags ->
            { model
                | browser = Browser.showUserTags (Dict.values model.userAddressTags) model.browser
            }
                |> n

        UserClickedRemoveAddress id ->
            { model
                | layers = Layer.removeAddress id model.layers
            }
                |> n

        UserClickedRemoveEntity id ->
            { model
                | layers = Layer.removeEntity id model.layers
                , browser =
                    case model.browser.type_ of
                        Browser.Entity (Loaded e) _ ->
                            if e.id == id then
                                model.browser |> s_visible False |> s_type_ Browser.None

                            else
                                model.browser

                        _ ->
                            model.browser
            }
                |> n

        UserClickedRemoveAddressLink id ->
            { model
                | layers = Layer.removeAddressLink id model.layers
            }
                |> n

        UserClickedRemoveEntityLink id ->
            { model
                | layers = Layer.removeEntityLink id model.layers
            }
                |> n

        UserClickedForceShowEntityLink id forceShow ->
            { model
                | layers = Layer.forceShowEntityLink id forceShow model.layers
            }
                |> n

        UserClickedAddressInEntityTagsTable entityId address ->
            ( model
            , BrowserGotAddressForEntity entityId
                |> GetAddressEffect
                    { address = address
                    , currency = Id.currency entityId
                    , includeActors = True
                    }
                |> ApiEffect
                |> List.singleton
            )

        UserClickedAddressInTable { address, currency } ->
            model
                |> s_selectIfLoaded Nothing
                |> loadAddress plugins
                    { currency = currency
                    , address = address
                    , table = Nothing
                    , at = Nothing
                    }

        BrowserGotAddressForEntity entityId address ->
            addAddressesAtEntity plugins uc entityId [ address ] model

        UserClickedAddressInEntityAddressesTable entityId address ->
            let
                added =
                    Layer.addAddressAtEntity
                        plugins
                        uc
                        entityId
                        address
                        { layers = model.layers
                        , new = Set.empty
                        , repositioned = Set.empty
                        }
            in
            ( { model
                | layers =
                    added.layers
                        |> addUserTag added.new model.userAddressTags
              }
                |> syncLinks added.repositioned
            , BrowserGotAddressTags
                { currency = address.currency
                , address = address.address
                }
                |> GetAddressTagsEffect
                    { currency = address.currency
                    , address = address.address
                    , pagesize = 10
                    , nextpage = Nothing
                    , includeBestClusterTag = False
                    }
                |> ApiEffect
                |> List.singleton
            )
                |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))

        UserClickedAddressInNeighborsTable addressId isOutgoing neighbor ->
            Layer.getAddress
                (Id.initAddressId
                    { layer = Id.layer addressId |> layerDelta isOutgoing
                    , currency = neighbor.address.currency
                    , id = neighbor.address.address
                    }
                )
                model.layers
                |> Maybe.map (\_ -> n model)
                |> Maybe.Extra.orElseLazy
                    (\_ ->
                        Layer.getAddress addressId model.layers
                            |> Maybe.map
                                (\address ->
                                    let
                                        entityId =
                                            Id.initEntityId
                                                { currency = Id.currency addressId
                                                , layer =
                                                    Id.layer addressId
                                                        |> layerDelta isOutgoing
                                                , id =
                                                    neighbor.address.entity
                                                }

                                        added =
                                            Layer.addAddressAtEntity plugins
                                                uc
                                                entityId
                                                neighbor.address
                                                { layers = model.layers
                                                , new = Set.empty
                                                , repositioned = Set.empty
                                                }
                                    in
                                    case Set.toList added.new of
                                        [] ->
                                            ( model
                                            , [ BrowserGotEntityForAddressNeighbor
                                                    { anchor = addressId
                                                    , isOutgoing = isOutgoing
                                                    , neighbors = [ neighbor ]
                                                    }
                                                    |> GetEntityEffect
                                                        { entity = neighbor.address.entity
                                                        , currency = Id.currency addressId
                                                        }
                                                    |> ApiEffect
                                              ]
                                            )

                                        addedAddressId :: _ ->
                                            Layer.getAddress addedAddressId added.layers
                                                |> Maybe.map
                                                    (\addedAddress ->
                                                        ( { model
                                                            | layers =
                                                                added.layers
                                                                    |> addUserTag added.new model.userAddressTags
                                                          }
                                                            |> addAddressLinks address isOutgoing [ ( neighbor, addedAddress ) ]
                                                            |> syncLinks added.repositioned
                                                        , [ BrowserGotAddressTags
                                                                { currency = Id.currency addressId
                                                                , address = Id.addressId addressId
                                                                }
                                                                |> GetAddressTagsEffect
                                                                    { currency = Id.currency addressId
                                                                    , address = Id.addressId addressId
                                                                    , pagesize = 10
                                                                    , nextpage = Nothing
                                                                    , includeBestClusterTag = False
                                                                    }
                                                                |> ApiEffect
                                                          ]
                                                        )
                                                            |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))
                                                    )
                                                |> Maybe.withDefault (n model)
                                )
                    )
                |> Maybe.withDefault (n model)

        UserClickedEntityInNeighborsTable entityId isOutgoing neighbor ->
            Layer.getEntity entityId model.layers
                |> Maybe.map
                    (\anchor ->
                        handleEntityNeighbors plugins uc anchor isOutgoing [ neighbor ] model
                    )
                |> Maybe.withDefault (n model)

        TagSearchMsg m ->
            model.tag
                |> Maybe.map
                    (\tag ->
                        let
                            ( tag_, effects ) =
                                Tag.searchMsg m tag
                        in
                        ( { model
                            | tag = Just tag_
                          }
                        , effects
                        )
                    )
                |> Maybe.withDefault (n model)

        UserClicksLegend id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Legend _, Just ( el, vis ) ) ->
                    toolVisible model el vis
                        |> n

                _ ->
                    getToolElement model id BrowserGotLegendElement

        BrowserGotLegendElement result ->
            makeLegend uc model
                |> toolElementResultToTool result model

        UserClicksConfiguraton id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Configuration _, Just ( el, vis ) ) ->
                    toolVisible model el vis
                        |> n

                _ ->
                    getToolElement model id BrowserGotConfigurationElement

        BrowserGotConfigurationElement result ->
            model.config
                |> Tool.Configuration
                |> toolElementResultToTool result model

        UserClickedExport id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Export, Just ( el, vis ) ) ->
                    toolVisible model el vis
                        |> n

                _ ->
                    getToolElement model id BrowserGotExportElement

        BrowserGotExportElement result ->
            toolElementResultToTool result model Tool.Export

        UserClickedImport id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Import, Just ( el, vis ) ) ->
                    toolVisible model el vis
                        |> n

                _ ->
                    getToolElement model id BrowserGotImportElement

        BrowserGotImportElement result ->
            toolElementResultToTool result model Tool.Import

        UserClickedHighlighter id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Highlighter, Just ( el, vis ) ) ->
                    toolVisible
                        (if vis then
                            deselectHighlighter model

                         else
                            model
                        )
                        el
                        vis
                        |> n

                _ ->
                    getToolElement model id BrowserGotHighlighterElement

        BrowserGotHighlighterElement result ->
            Tool.Highlighter
                |> toolElementResultToTool result model

        UserClickedHighlightColor color ->
            let
                highlights =
                    Highlighter.selectColor color model.highlights
            in
            { model
                | highlights = highlights
                , config =
                    model.config
                        |> s_highlighter (highlights.selected /= Nothing)
            }
                |> n

        UserClicksHighlight i ->
            let
                highlights =
                    Highlighter.selectHighlight i model.highlights
            in
            { model
                | highlights = highlights
                , config =
                    model.config
                        |> s_highlighter (highlights.selected /= Nothing)
            }
                |> n

        UserClickedHighlightTrash i ->
            { model
                | highlights =
                    Highlighter.removeHighlight i model.highlights
                , layers =
                    Highlighter.getColor i model.highlights
                        |> Maybe.map
                            (\color ->
                                Layer.updateEntityColor color Nothing model.layers
                                    |> Layer.updateAddressColor color Nothing
                            )
                        |> Maybe.withDefault model.layers
            }
                |> n

        UserInputsHighlightTitle i title ->
            { model
                | highlights =
                    Highlighter.setHighlightTitle i title model.highlights
            }
                |> n

        UserChangesCurrency _ ->
            -- handled upstream
            n model

        UserChangesValueDetail _ ->
            -- handled upstream
            n model

        UserChangesAddressLabelType at ->
            { model
                | config =
                    model.config
                        |> s_addressLabelType
                            (Config.Graph.stringToAddressLabel at
                                |> Maybe.withDefault model.config.addressLabelType
                            )
            }
                |> n

        UserChangesTxLabelType at ->
            { model
                | config =
                    model.config
                        |> s_txLabelType
                            (case at of
                                "notxs" ->
                                    Config.Graph.NoTxs

                                "value" ->
                                    Config.Graph.Value

                                _ ->
                                    model.config.txLabelType
                            )
            }
                |> n

        UserClickedSearch id ->
            let
                ( search, cmd ) =
                    Search.init uc.allConcepts id
            in
            ( { model
                | search = search |> Just
              }
            , CmdEffect cmd
                |> List.singleton
            )

        UserSelectsDirection direction ->
            updateSearch (Search.selectDirection direction) model

        UserSelectsCriterion criterion ->
            updateSearch
                (Search.selectCriterion
                    { categories = uc.allConcepts
                    }
                    criterion
                )
                model

        UserSelectsSearchCategory category ->
            updateSearch (Search.selectCategory category) model

        UserInputsSearchDepth input ->
            updateSearch
                (\s ->
                    n
                        { s
                            | depth = input
                        }
                )
                model

        UserInputsSearchBreadth input ->
            updateSearch
                (\s ->
                    n
                        { s
                            | breadth = input
                        }
                )
                model

        UserInputsSearchMaxAddresses input ->
            updateSearch
                (\s ->
                    n
                        { s
                            | maxAddresses = input
                        }
                )
                model

        UserSubmitsSearchInput ->
            model.search
                |> Maybe.andThen
                    (\search ->
                        Maybe.map3
                            (\depth breadth maxAddresses ->
                                Search.submit { depth = depth, breadth = breadth, maxAddresses = maxAddresses } search
                            )
                            (String.toInt search.depth)
                            (String.toInt search.breadth)
                            (String.toInt search.maxAddresses)
                    )
                |> Maybe.map (mapFirst (\_ -> { model | search = Nothing }))
                |> Maybe.withDefault (n model)

        UserClicksCloseSearchHovercard ->
            { model
                | search = Nothing
            }
                |> n

        BrowserGotEntitySearchResult id isOutgoing result ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\anchor ->
                        handleEntitySearchResult
                            plugins
                            uc
                            anchor
                            (prepareSearchResult result)
                            isOutgoing
                            ( model, [] )
                    )
                |> Maybe.withDefault (n model)

        UserClickedExportGraphics _ ->
            -- handled upstream
            n model

        UserClickedExportTagPack _ ->
            -- handled upstream
            n model

        UserClickedImportTagPack ->
            ( model
            , File.Select.file [ "text/yaml" ] BrowserGotTagPackFile
                |> CmdEffect
                |> List.singleton
            )

        BrowserGotTagPackFile file ->
            ( model
            , File.toString file
                |> Task.map
                    (Yaml.Decode.fromString
                        (Yaml.Decode.list decodeYamlTag
                            |> Yaml.Decode.field "tags"
                        )
                    )
                |> Task.perform (BrowserReadTagPackFile (File.name file))
                |> CmdEffect
                |> List.singleton
            )

        BrowserReadTagPackFile _ _ ->
            -- handled upstream
            n model

        UserClickedImportGS ->
            ( model
            , Ports.deserialize ()
                |> CmdEffect
                |> List.singleton
            )

        UserClickedExportGS _ ->
            -- handled upstream
            n model

        PortDeserializedGS _ ->
            -- handled upstream
            n model

        BrowserGotBulkAddresses currency deserializing addresses ->
            let
                entities =
                    ((deserializing.deserialized.entities
                        |> List.filterMap
                            (\e ->
                                case e.rootAddress of
                                    Nothing ->
                                        if e.noAddresses == 0 then
                                            e.id |> Id.entityId |> Just

                                        else
                                            Nothing

                                    Just _ ->
                                        Nothing
                            )
                     )
                        ++ List.map .entity addresses
                    )
                        |> List.foldl Set.insert Set.empty
                        |> Set.toList

                toMsg theMsg =
                    deserializing
                        |> s_addresses addresses
                        |> theMsg currency
            in
            ( model
            , [ if List.isEmpty entities then
                    Task.succeed []
                        |> Task.perform (toMsg BrowserGotBulkEntities)
                        |> CmdEffect

                else
                    toMsg BrowserGotBulkEntities
                        |> BulkGetEntityEffect
                            { currency = currency
                            , entities = entities
                            }
                        |> ApiEffect
              ]
            )

        BrowserGotBulkEntities currency deserializing entities ->
            let
                rootAddresses =
                    deserializing.deserialized.entities
                        |> List.filterMap .rootAddress

                toMsg theMsg =
                    deserializing
                        |> s_entities entities
                        |> theMsg currency
            in
            ( model
            , [ if List.isEmpty rootAddresses then
                    Task.succeed []
                        |> Task.perform (toMsg BrowserGotBulkAddressEntities)
                        |> CmdEffect

                else
                    toMsg BrowserGotBulkAddressEntities
                        |> BulkGetAddressEntityEffect
                            { currency = currency
                            , addresses = rootAddresses
                            }
                        |> ApiEffect
              ]
            )

        BrowserGotBulkAddressEntities currency deserializing ents ->
            let
                entities =
                    ents |> List.map second

                deser =
                    deserializing
                        |> s_entities (entities ++ deserializing.entities)

                acc =
                    deser
                        |> Layer.deserialize plugins uc

                tags =
                    deserializing.deserialized.addresses
                        |> List.filterMap .userTag

                entityTags =
                    deserializing.deserialized.entities
                        |> List.filterMap .userTag
                        |> List.filterMap (normalizeDeserializedEntityTag deserializing.entities)

                colorfulAddresses =
                    deserializing.deserialized.addresses
                        |> List.filterMap
                            (\a ->
                                a.color
                                    |> Maybe.map (pair a.id)
                            )

                colorfulEntities =
                    deserializing.deserialized.entities
                        |> List.filterMap
                            (\a ->
                                a.color
                                    |> Maybe.map (pair a.id)
                            )
            in
            ( tags
                ++ entityTags
                |> List.foldl (storeUserTag uc)
                    { model
                        | layers =
                            colorfulEntities
                                |> List.foldl
                                    (\( id, color ) ->
                                        Layer.updateEntity id (Entity.updateColor color)
                                            >> Layer.syncLinks [ id ]
                                    )
                                    (colorfulAddresses
                                        |> List.foldl
                                            (\( id, color ) ->
                                                Layer.updateAddress id (Address.updateColor color)
                                                    >> Layer.syncLinks
                                                        (Layer.getAddress id model.layers
                                                            |> Maybe.map .entityId
                                                            |> Maybe.map List.singleton
                                                            |> Maybe.withDefault []
                                                        )
                                            )
                                            (IntDict.union acc.layers model.layers)
                                    )
                    }
                |> insertEntityShadowLinks acc.newEntityIds
                |> insertAddressShadowLinks acc.newAddressIds
            , [ BrowserGotBulkEntityNeighbors currency True
                    |> BulkGetEntityNeighborsEffect
                        { currency = currency
                        , isOutgoing = True
                        , entities = List.map .entity deser.entities
                        , onlyIds = True
                        }
                    |> ApiEffect
              , BrowserGotBulkAddressNeighbors currency True
                    |> BulkGetAddressNeighborsEffect
                        { currency = currency
                        , isOutgoing = True
                        , addresses = List.map .address deser.addresses
                        , onlyIds = Just <| List.map .address deser.addresses
                        }
                    |> ApiEffect
              , InternalGraphAddedAddressesEffect acc.newAddressIds
              , InternalGraphAddedEntitiesEffect acc.newEntityIds
              ]
                ++ (deserializing.addresses
                        |> List.map
                            (\address ->
                                BrowserGotAddressTags
                                    { currency = address.currency
                                    , address = address.address
                                    }
                                    |> GetAddressTagsEffect
                                        { currency = address.currency
                                        , address = address.address
                                        , pagesize = 10
                                        , nextpage = Nothing
                                        , includeBestClusterTag = False
                                        }
                                    |> ApiEffect
                            )
                   )
            )

        BrowserGotBulkEntityNeighbors currency isOutgoing entityNeighbors ->
            entityNeighbors
                |> List.Extra.gatherEqualsBy first
                |> List.map (\( fst, more ) -> ( first fst, second fst :: List.map second more ))
                |> List.foldl
                    (\( requestEntity, neighbors ) model_ ->
                        Layer.getEntities currency requestEntity model_.layers
                            |> List.foldl
                                (\anchor model__ ->
                                    let
                                        neighborsWithEntity =
                                            neighbors
                                                |> List.filterMap
                                                    (\neighbor ->
                                                        let
                                                            entityId =
                                                                Id.initEntityId
                                                                    { currency = currency
                                                                    , id = neighbor.entity.entity
                                                                    , layer =
                                                                        Id.layer anchor.id
                                                                            |> layerDelta isOutgoing
                                                                    }
                                                        in
                                                        Layer.getEntity entityId model__.layers
                                                            |> Maybe.map (pair neighbor)
                                                    )
                                    in
                                    addEntityLinks anchor isOutgoing neighborsWithEntity model__
                                )
                                model_
                    )
                    model
                |> n

        BrowserGotBulkAddressNeighbors currency isOutgoing addressNeighbors ->
            addressNeighbors
                |> List.Extra.gatherEqualsBy first
                |> List.map (\( fst, more ) -> ( first fst, second fst :: List.map second more ))
                |> List.foldl
                    (\( requestAddress, neighbors ) model_ ->
                        Layer.getAddresses { currency = currency, address = requestAddress } model_.layers
                            |> List.foldl
                                (\anchor model__ ->
                                    let
                                        neighborsWithAddress =
                                            neighbors
                                                |> List.filterMap
                                                    (\neighbor ->
                                                        let
                                                            addressId =
                                                                Id.initAddressId
                                                                    { currency = currency
                                                                    , id = neighbor.address.address
                                                                    , layer =
                                                                        Id.layer anchor.id
                                                                            |> layerDelta isOutgoing
                                                                    }
                                                        in
                                                        Layer.getAddress addressId model__.layers
                                                            |> Maybe.map (pair neighbor)
                                                    )
                                    in
                                    addAddressLinks anchor isOutgoing neighborsWithAddress model__
                                )
                                model_
                    )
                    model
                |> n

        BrowserGotBulkAddressTags _ tags ->
            (tags
                |> List.Extra.groupWhile
                    (\t1 t2 -> t1.currency == t2.currency && t1.address == t2.address)
                |> List.foldl
                    (\( fst, more ) ->
                        updateAddresses
                            { currency = String.toLower fst.currency
                            , address = fst.address
                            }
                            (Address.updateTags (fst :: more))
                    )
                    model
            )
                |> n

        UserClickedUndo ->
            undoRedo History.undo model

        UserClickedRedo ->
            undoRedo History.redo model

        UserClickedNew ->
            -- handled upstream
            n model

        UserClickedNewYes ->
            -- handled upstream
            n model

        UserInputsFilterTable input ->
            ( { model
                | browser = Browser.searchTable model.config (Table.Update input) model.browser
              }
            , Dom.focus "tableFilter"
                |> Task.attempt (\_ -> NoOp)
                |> CmdEffect
                |> List.singleton
            )

        UserClickedFitGraph ->
            Layer.getBoundingBox model.layers
                |> Maybe.map (updateTransformByBoundingBox uc model)
                |> Maybe.withDefault model
                |> n

        UserClickedShowEntityShadowLinks ->
            { model
                | config =
                    model.config
                        |> s_showEntityShadowLinks (not model.config.showEntityShadowLinks)
            }
                |> n

        UserClickedShowAddressShadowLinks ->
            { model
                | config =
                    model.config
                        |> s_showAddressShadowLinks (not model.config.showAddressShadowLinks)
            }
                |> n

        UserClickedToggleShowDatesInUserLocale ->
            -- handled upstream
            n model

        UserClickedToggleShowZeroTransactions ->
            let
                gc =
                    model.config
                        |> s_showZeroTransactions (not model.config.showZeroTransactions)
            in
            { model
                | config = gc
                , browser = Browser.filterTable gc model.browser
            }
                |> n

        UserPressesDelete ->
            { model
                | layers =
                    case model.selected of
                        SelectedAddress id ->
                            Layer.removeAddress id model.layers

                        SelectedEntity id ->
                            Layer.removeEntity id model.layers

                        SelectedAddresslink id ->
                            Layer.removeAddressLink id model.layers

                        SelectedEntitylink id ->
                            Layer.removeEntityLink id model.layers

                        SelectedNone ->
                            model.layers
            }
                |> n

        UserClickedTagsFlag id ->
            ( model
            , Route.entityRoute
                { currency = Id.currency id
                , entity = Id.entityId id
                , table = Just Route.EntityTagsTable
                , layer = Id.layer id |> Just
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserClicksDownloadCSVInTable ->
            ( model
            , Browser.tableAsCSV uc.locale uc model.browser
                |> Maybe.map (DownloadCSVEffect >> List.singleton)
                |> Maybe.withDefault []
            )

        UserClickedExternalLink url ->
            ( model, Ports.newTab url |> CmdEffect |> List.singleton )

        AnimationFrameDeltaForTransform delta ->
            { model
                | transform = Transform.transition delta model.transform
            }
                |> repositionHovercards

        SearchHovercardMsg hm ->
            model.search
                |> Maybe.map
                    (\s ->
                        let
                            ( search, cmd ) =
                                Hovercard.update hm s.hovercard
                        in
                        ( { model
                            | search = s |> s_hovercard search |> Just
                          }
                        , Cmd.map SearchHovercardMsg cmd
                            |> CmdEffect
                            |> List.singleton
                        )
                    )
                |> Maybe.withDefault (n model)

        TagHovercardMsg hm ->
            model.tag
                |> Maybe.map
                    (\s ->
                        let
                            ( tag, cmd ) =
                                Hovercard.update hm s.hovercard
                        in
                        ( { model
                            | tag = s |> s_hovercard tag |> Just
                          }
                        , Cmd.map TagHovercardMsg cmd
                            |> CmdEffect
                            |> List.singleton
                        )
                    )
                |> Maybe.withDefault (n model)

        NoOp ->
            n model


type alias SearchResult =
    { matchingAddresses : List Api.Data.Address
    , neighbor : Api.Data.NeighborEntity
    , paths : More
    }


type More
    = More (List SearchResult)


prepareSearchResult : List Api.Data.SearchResultLevel1 -> List SearchResult
prepareSearchResult paths =
    paths
        |> List.map
            (\p1 ->
                { matchingAddresses = p1.matchingAddresses
                , neighbor = p1.neighbor
                , paths =
                    p1.paths
                        |> List.map
                            (\p2 ->
                                { matchingAddresses = p2.matchingAddresses
                                , neighbor = p2.neighbor
                                , paths =
                                    p2.paths
                                        |> List.map
                                            (\p3 ->
                                                { matchingAddresses = p3.matchingAddresses
                                                , neighbor = p3.neighbor
                                                , paths =
                                                    p3.paths
                                                        |> List.map
                                                            (\p4 ->
                                                                { matchingAddresses = p4.matchingAddresses
                                                                , neighbor = p4.neighbor
                                                                , paths =
                                                                    p4.paths
                                                                        |> List.map
                                                                            (\p5 ->
                                                                                { matchingAddresses = p5.matchingAddresses
                                                                                , neighbor = p5.neighbor
                                                                                , paths =
                                                                                    p5.paths
                                                                                        |> List.map
                                                                                            (\p6 ->
                                                                                                { matchingAddresses = p6.matchingAddresses
                                                                                                , neighbor = p6.neighbor
                                                                                                , paths =
                                                                                                    p6.paths
                                                                                                        |> List.map
                                                                                                            (\p7 ->
                                                                                                                { matchingAddresses = p7.matchingAddresses
                                                                                                                , neighbor = p7.neighbor
                                                                                                                , paths = More []
                                                                                                                }
                                                                                                            )
                                                                                                        |> More
                                                                                                }
                                                                                            )
                                                                                        |> More
                                                                                }
                                                                            )
                                                                        |> More
                                                                }
                                                            )
                                                        |> More
                                                }
                                            )
                                        |> More
                                }
                            )
                        |> More
                }
            )


handleEntitySearchResult : Plugins -> Update.Config -> Entity -> List SearchResult -> Bool -> ( Model, List Effect ) -> ( Model, List Effect )
handleEntitySearchResult plugins uc anchor nodes isOutgoing ( model, effects ) =
    let
        neighbors =
            nodes
                |> List.map .neighbor
    in
    nodes
        |> List.foldl
            (\{ neighbor, paths } ( mo, eff ) ->
                let
                    id =
                        Id.initEntityId
                            { currency = neighbor.entity.currency
                            , id = neighbor.entity.entity
                            , layer =
                                Id.layer anchor.id |> layerDelta isOutgoing
                            }
                in
                Layer.getEntity id mo.layers
                    |> Maybe.map
                        (\ancho ->
                            case paths of
                                More ps ->
                                    handleEntitySearchResult plugins uc ancho ps isOutgoing ( mo, eff )
                        )
                    |> Maybe.withDefault ( mo, eff )
            )
            (handleEntityNeighbors plugins uc anchor isOutgoing neighbors model)
        |> mapSecond ((++) effects)


updateSearch : (Search.Model -> ( Search.Model, List Effect )) -> Model -> ( Model, List Effect )
updateSearch upd model =
    model.search
        |> Maybe.map
            (\search ->
                let
                    ( s, eff ) =
                        upd search
                in
                ( { model
                    | search = Just s
                  }
                , eff
                )
            )
        |> Maybe.withDefault (n model)


toolVisible : Model -> Dom.Element -> Bool -> Model
toolVisible model element visible =
    { model
        | activeTool =
            model.activeTool
                |> s_element (Just ( element, not visible ))
    }


getToolElement : Model -> String -> (Result Dom.Error Dom.Element -> Msg) -> ( Model, List Effect )
getToolElement model id msg =
    ( model
    , id
        |> Dom.getElement
        |> Task.attempt msg
        |> CmdEffect
        |> List.singleton
    )


toolElementResultToTool : Result Dom.Error Dom.Element -> Model -> Tool.Toolbox -> ( Model, List Effect )
toolElementResultToTool result model toolbox =
    result
        |> Result.map
            (\element ->
                { model
                    | activeTool =
                        { toolbox = toolbox
                        , element = Just ( element, True )
                        }
                }
            )
        |> Result.withDefault model
        |> n


hideContextmenu : Model -> ( Model, List Effect )
hideContextmenu model =
    n { model | contextMenu = Nothing }


updateByRoute : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute plugins route model =
    forcePushHistory model
        |> s_route route
        |> updateByRoute_ plugins route


updateByRoute_ : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute_ plugins route model =
    case route |> Log.log "route" of
        Route.Root ->
            deselect model
                |> n

        Route.Currency currency (Route.Address a table layer) ->
            model
                |> s_selectIfLoaded (Just (SelectAddress { currency = currency, address = a }))
                |> loadAddress plugins
                    { currency = currency
                    , address = a
                    , table = table
                    , at = Maybe.map AtLayer layer
                    }

        Route.Currency currency (Route.AddressPath ( address, addresses )) ->
            let
                a =
                    normalizeEth currency address
            in
            model
                |> s_selectIfLoaded (Just (SelectAddress { currency = currency, address = a }))
                |> loadAddressPath plugins
                    { currency = currency
                    , addresses = address :: List.map (normalizeEth currency) addresses
                    }

        Route.Currency currency (Route.Entity e table layer) ->
            model
                |> s_selectIfLoaded (Just (SelectEntity { currency = currency, entity = e }))
                |> loadEntity plugins
                    { currency = currency
                    , entity = e
                    , table = table
                    , layer = layer
                    }

        Route.Currency currency (Route.Tx t table tokenTxId) ->
            let
                ( browser, effect ) =
                    if Data.isAccountLike currency || tokenTxId /= Nothing then
                        Browser.loadingTxAccount { currency = currency, txHash = t, tokenTxId = tokenTxId } currency model.browser

                    else
                        Browser.loadingTxUtxo { currency = currency, txHash = t } model.browser

                ( browser2, effects ) =
                    if Data.isAccountLike currency then
                        table
                            |> Maybe.map (\tb -> Browser.showTxAccountTable tb browser)
                            |> Maybe.withDefault (Browser.hideTable browser |> n)

                    else if tokenTxId == Nothing then
                        table
                            |> Maybe.map (\tb -> Browser.showTxUtxoTable tb browser)
                            |> Maybe.withDefault (Browser.hideTable browser |> n)

                    else
                        n browser
            in
            ( { model
                | browser = browser2
              }
            , effect ++ effects
            )

        Route.Currency currency (Route.Block b table) ->
            let
                ( browser1, effects1 ) =
                    Browser.loadingBlock { currency = currency, block = b } model.browser

                ( browser2, effects2 ) =
                    table
                        |> Maybe.map (\tb -> Browser.showBlockTable tb browser1)
                        |> Maybe.withDefault (Browser.hideTable browser1 |> n)
            in
            ( { model
                | browser = browser2
              }
            , effects1
                ++ effects2
            )

        Route.Currency currency (Route.Addresslink src srcLayer dst dstLayer table) ->
            let
                s =
                    Id.initAddressId { id = src, layer = srcLayer, currency = currency }

                t =
                    Id.initAddressId { id = dst, layer = dstLayer, currency = currency }
            in
            Layer.getAddress s model.layers
                |> Maybe.andThen
                    (\source ->
                        (case source.links of
                            Address.Links links ->
                                Dict.get t links
                        )
                            |> Maybe.map (\link -> selectAddressLink table source link model)
                    )
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        model
                            |> s_selectIfLoaded (Just (SelectAddresslink table { currency = currency, address = src } { currency = currency, address = dst }))
                            |> loadAddressPath plugins
                                { currency = currency
                                , addresses = [ src, dst ]
                                }
                    )

        Route.Currency currency (Route.Entitylink src srcLayer dst dstLayer table) ->
            let
                s =
                    Id.initEntityId { id = src, layer = srcLayer, currency = currency }

                t =
                    Id.initEntityId { id = dst, layer = dstLayer, currency = currency }
            in
            Layer.getEntity s model.layers
                |> Maybe.andThen
                    (\source ->
                        (case source.links of
                            Entity.Links links ->
                                Dict.get t links
                        )
                            |> Maybe.map
                                (\link -> selectEntityLink table source link model)
                    )
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        model
                            |> s_selectIfLoaded (Just (SelectEntitylink table { currency = currency, entity = src } { currency = currency, entity = dst }))
                            |> loadEntityPath plugins
                                { currency = currency
                                , entities = [ src, dst ]
                                }
                    )

        Route.Label l ->
            let
                ( browser, effect ) =
                    Browser.loadingLabel l model.browser
            in
            ( { model
                | browser = browser
              }
            , effect
            )

        Route.Actor actorId table ->
            let
                getActorAction =
                    [ BrowserGotActor
                        |> GetActorEffect
                            { actorId = actorId }
                        |> ApiEffect
                    ]

                ( newbrowser, effectsActor ) =
                    case model.browser.type_ of
                        Browser.Actor (Loading currentActorId _) _ ->
                            if currentActorId /= actorId then
                                ( Browser.loadingActor actorId model.browser, getActorAction )

                            else
                                ( Browser.openActor True model.browser, [] )

                        Browser.Actor (Loaded actor) _ ->
                            if actor.id /= actorId then
                                ( Browser.loadingActor actorId model.browser, getActorAction )

                            else
                                ( Browser.openActor True model.browser, [] )

                        _ ->
                            ( Browser.loadingActor actorId model.browser, getActorAction )

                ( browser2, effectsTagsActor ) =
                    table
                        |> Maybe.map (\tb -> Browser.showActorTable tb newbrowser)
                        |> Maybe.withDefault (n newbrowser)
            in
            ( { model
                | browser = browser2
              }
            , effectsActor ++ effectsTagsActor
            )

        Route.Plugin _ ->
            n model


addAddressNeighborsWithEntity : Plugins -> Update.Config -> ( Address, Entity ) -> Bool -> ( List Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> { model : Model, newAddresses : List Address, newEntities : List EntityId, repositioned : Set EntityId }
addAddressNeighborsWithEntity plugins uc ( anchorAddress, anchorEntity ) isOutgoing ( neighbors, entity ) model =
    let
        acc =
            Layer.addEntityNeighbors plugins uc anchorEntity isOutgoing [ entity ] model.layers
    in
    Set.toList acc.new
        |> List.head
        |> Maybe.map
            (\new ->
                let
                    added =
                        neighbors
                            |> List.foldl
                                (\neighbor added_ ->
                                    let
                                        added__ =
                                            Layer.addAddressAtEntity plugins uc new neighbor.address added_
                                    in
                                    { layers =
                                        added__.layers
                                            |> addUserTag added__.new model.userAddressTags
                                    , new = added__.new
                                    , repositioned = added__.repositioned
                                    }
                                )
                                { layers = acc.layers
                                , new = Set.empty
                                , repositioned = acc.repositioned
                                }
                in
                { model =
                    { model
                        | layers = added.layers
                    }
                , newAddresses =
                    Set.toList added.new
                        |> List.filterMap
                            (\a -> Layer.getAddress a added.layers)
                , newEntities = [ new ]
                , repositioned = added.repositioned
                }
            )
        |> Maybe.withDefault
            { model = model
            , newAddresses = []
            , newEntities = []
            , repositioned = acc.repositioned
            }


addEntityNeighbors : Plugins -> Update.Config -> Entity -> Bool -> List Api.Data.NeighborEntity -> Model -> ( Model, List ( Api.Data.NeighborEntity, Entity ), Set EntityId )
addEntityNeighbors plugins uc anchor isOutgoing neighbors model =
    let
        acc =
            Layer.addEntityNeighbors plugins uc anchor isOutgoing (List.map .entity neighbors) model.layers

        aligned =
            neighbors
                |> List.filterMap
                    (\neighbor ->
                        let
                            entityId =
                                Id.initEntityId
                                    { currency = anchor.entity.currency
                                    , layer =
                                        Id.layer anchor.id |> layerDelta isOutgoing
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


addAddressLinks : Address -> Bool -> List ( Api.Data.NeighborAddress, Address ) -> Model -> Model
addAddressLinks anchor isOutgoing neighbors model =
    let
        linkData =
            neighbors
                |> List.map (mapFirst Link.fromNeighbor)

        layers =
            if isOutgoing then
                Layer.updateAddressLinks { currency = Id.currency anchor.id, address = Id.addressId anchor.id } linkData model.layers

            else
                linkData
                    |> List.foldl
                        (\( linkDatum, neighbor ) ->
                            Layer.updateAddressLinks
                                { currency = Id.currency neighbor.id, address = Id.addressId neighbor.id }
                                [ ( linkDatum, anchor ) ]
                        )
                        model.layers
    in
    { model
        | layers = layers
    }


addEntityEgonet : String -> Int -> Bool -> List Api.Data.NeighborEntity -> Model -> Model
addEntityEgonet currency entity isOutgoing neighbors model =
    let
        entities =
            List.concatMap
                (\neighbor ->
                    Layer.getEntities neighbor.entity.currency neighbor.entity.entity model.layers
                        |> List.map (pair neighbor)
                )
                neighbors

        anchors =
            Layer.getEntities currency entity model.layers
    in
    List.foldl
        (\anchor model_ ->
            addEntityLinks anchor isOutgoing entities model_
        )
        model
        anchors


handleEntityNeighbors : Plugins -> Update.Config -> Entity -> Bool -> List Api.Data.NeighborEntity -> Model -> ( Model, List Effect )
handleEntityNeighbors plugins uc anchor isOutgoing neighbors model =
    let
        ( newModel, new, repositioned ) =
            addEntityNeighbors plugins uc anchor isOutgoing neighbors model
    in
    ( addEntityLinks anchor isOutgoing new newModel
        |> syncLinks repositioned
        |> insertEntityShadowLinks (List.map (second >> .id) new |> Set.fromList)
    , neighbors
        |> List.concatMap
            (\{ entity } ->
                getEntityEgonet
                    { currency = entity.currency
                    , entity = entity.entity
                    }
                    BrowserGotEntityEgonet
                    newModel.layers
                    |> List.map ApiEffect
            )
        |> (::)
            (new
                |> List.map (second >> .id)
                |> Set.fromList
                |> InternalGraphAddedEntitiesEffect
            )
    )


{-|

    neighbors contains a list of address neighbors and their parent entity.

-}
handleAddressNeighbor : Plugins -> Update.Config -> ( Address, Entity ) -> Bool -> ( List Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> ( Model, List Effect )
handleAddressNeighbor plugins uc anchor isOutgoing neighbors model =
    let
        added =
            addAddressNeighborsWithEntity plugins uc anchor isOutgoing neighbors model
    in
    ( added.newAddresses
        |> List.foldl
            (\address model_ ->
                first neighbors
                    |> List.Extra.find (\n -> n.address.currency == address.address.currency && n.address.address == address.address.address)
                    |> Maybe.map
                        (\neighbor ->
                            addAddressLinks (first anchor) isOutgoing [ ( neighbor, address ) ] model_
                        )
                    |> Maybe.withDefault model_
            )
            added.model
        |> syncLinks added.repositioned
        |> insertAddressShadowLinks (List.map .id added.newAddresses |> Set.fromList)
    , first neighbors
        |> List.map
            (\neighbor ->
                BrowserGotAddressTags
                    { currency = neighbor.address.currency
                    , address = neighbor.address.address
                    }
                    |> GetAddressTagsEffect
                        { currency = neighbor.address.currency
                        , address = neighbor.address.address
                        , pagesize = 10
                        , nextpage = Nothing
                        , includeBestClusterTag = False
                        }
                    |> ApiEffect
            )
        |> (++)
            (added.newAddresses
                |> List.map .id
                |> List.concatMap
                    (\a ->
                        getAddressEgonet a BrowserGotAddressEgonet added.model.layers
                            |> List.map ApiEffect
                    )
            )
        |> (::)
            (added.newAddresses
                |> List.map .id
                |> Set.fromList
                |> InternalGraphAddedAddressesEffect
            )
        |> (::)
            (added.newEntities
                |> Set.fromList
                |> InternalGraphAddedEntitiesEffect
            )
    )


syncLinks : Set EntityId -> Model -> Model
syncLinks repositioned model =
    let
        ids =
            Set.toList repositioned
    in
    { model
        | layers =
            Layer.syncLinks ids model.layers
                |> Layer.insertEntityShadowLinks ids
    }


insertEntityShadowLinks : Set EntityId -> Model -> Model
insertEntityShadowLinks new model =
    let
        ids =
            Set.toList new
    in
    { model
        | layers = Layer.insertEntityShadowLinks ids model.layers
    }


insertAddressShadowLinks : Set AddressId -> Model -> Model
insertAddressShadowLinks new model =
    let
        ids =
            Set.toList new
    in
    { model
        | layers = Layer.insertAddressShadowLinks ids model.layers
    }


selectAddress : Address -> Maybe Route.AddressTable -> Model -> ( Model, List Effect )
selectAddress address table model =
    if model.selectIfLoaded /= Just (SelectAddress { currency = address.address.currency, address = address.address.address }) then
        n model

    else
        let
            ( browser1, effects1 ) =
                Browser.showAddress address model.browser

            ( browser2, effects2 ) =
                table
                    |> Maybe.map (\t -> Browser.showAddressTable t browser1)
                    |> Maybe.withDefault (Browser.hideTable browser1 |> n)

            newmodel =
                deselect model
        in
        ( { newmodel
            | browser = browser2
            , selected = SelectedAddress address.id
            , selectIfLoaded = Nothing
            , layers = Layer.selectAddress address.id newmodel.layers
          }
        , InternalGraphSelectedAddressEffect address.id :: effects1 ++ effects2
        )


selectEntity : Entity -> Maybe Route.EntityTable -> Model -> ( Model, List Effect )
selectEntity entity table model =
    if model.selectIfLoaded /= Just (SelectEntity { currency = entity.entity.currency, entity = entity.entity.entity }) then
        n model

    else
        let
            ( browser1, effects1 ) =
                Browser.showEntity entity model.browser

            ( browser2, effects2 ) =
                table
                    |> Maybe.map (\t -> Browser.showEntityTable t browser1)
                    |> Maybe.withDefault (Browser.hideTable browser1 |> n)

            newmodel =
                deselect model
        in
        ( { newmodel
            | browser = browser2
            , selected = SelectedEntity entity.id
            , selectIfLoaded = Nothing
            , layers =
                Layer.selectEntity entity.id newmodel.layers
          }
        , effects1 ++ effects2
        )


deselect : Model -> Model
deselect model =
    { model
        | selected = SelectedNone
        , browser =
            model.browser
                |> s_visible False
        , layers =
            deselectLayers model.selected model.layers
    }


deselectLayers : Selected -> IntDict Layer -> IntDict Layer
deselectLayers selected layers =
    case selected of
        SelectedEntity id ->
            Layer.updateEntity id
                (\e -> { e | selected = False })
                layers

        SelectedAddress id ->
            Layer.updateAddress id
                (\e -> { e | selected = False })
                layers

        SelectedAddresslink id ->
            Layer.updateAddressLink id (\e -> { e | selected = False }) layers

        SelectedEntitylink id ->
            Layer.updateEntityLink id (\e -> { e | selected = False }) layers

        SelectedNone ->
            layers


draggingToClick : Coords -> Coords -> Bool
draggingToClick start current =
    Coords.betrag start current < 2


storeUserTag : Update.Config -> Tag.UserTag -> Model -> Model
storeUserTag uc tag model =
    if String.isEmpty tag.label then
        model

    else
        let
            flag =
                if tag.isClusterDefiner then
                    "entity"

                else
                    "address"

            userAddressTags =
                Dict.insert ( tag.currency, tag.address, flag ) tag model.userAddressTags
        in
        { model
            | userAddressTags =
                userAddressTags
            , tag = Nothing
            , browser = Browser.updateUserTags (Dict.values userAddressTags) model.browser
        }
            |> updateLegend uc
            |> updateAddresses { currency = tag.currency, address = tag.address }
                (\a ->
                    { a
                        | userTag =
                            if a.userTag == Nothing || not tag.isClusterDefiner then
                                Just tag

                            else
                                a.userTag
                    }
                )
            |> updateEntitiesIf
                (\e ->
                    tag.isClusterDefiner
                        && e.entity.currency
                        == tag.currency
                        && e.entity.rootAddress
                        == tag.address
                )
                (\a -> { a | userTag = Just tag })


deleteUserTag : Update.Config -> Tag.UserTag -> Model -> Model
deleteUserTag uc tag model =
    let
        flag =
            if tag.isClusterDefiner then
                "entity"

            else
                "address"

        userAddressTags =
            Dict.remove ( tag.currency, tag.address, flag ) model.userAddressTags
    in
    { model
        | userAddressTags =
            userAddressTags
        , tag = Nothing
        , browser = Browser.updateUserTags (Dict.values userAddressTags) model.browser
    }
        |> updateLegend uc
        |> updateAddresses { currency = tag.currency, address = tag.address }
            (\a ->
                { a
                    | userTag =
                        if Maybe.map .isClusterDefiner a.userTag == Just tag.isClusterDefiner then
                            Nothing

                        else
                            a.userTag
                }
            )
        |> updateEntitiesIf
            (\e ->
                tag.isClusterDefiner
                    && e.entity.currency
                    == tag.currency
                    && e.entity.rootAddress
                    == tag.address
            )
            (\a -> { a | userTag = Nothing })


updateLegend : Update.Config -> Model -> Model
updateLegend uc model =
    { model
        | activeTool =
            case model.activeTool.toolbox of
                Tool.Legend _ ->
                    model.activeTool
                        |> s_toolbox (makeLegend uc model)

                _ ->
                    model.activeTool
    }


updateAddresses : A.Address -> (Address -> Address) -> Model -> Model
updateAddresses id upd model =
    { model
        | layers = Layer.updateAddresses id upd model.layers
        , browser = Browser.updateAddress id upd model.browser
    }


updateEntitiesIf : (Entity -> Bool) -> (Entity -> Entity) -> Model -> Model
updateEntitiesIf predicate upd model =
    { model
        | layers = Layer.updateEntitiesIf predicate upd model.layers
        , browser = Browser.updateEntityIf predicate upd model.browser
    }


updateByPluginOutMsg : Plugins -> List Plugin.OutMsg -> Model -> ( Model, List Effect )
updateByPluginOutMsg plugins outMsgs model =
    outMsgs
        |> List.foldl
            (\msg ( mo, eff ) ->
                case Log.log "outMsg" msg of
                    PluginInterface.ShowBrowser ->
                        ( { mo
                            | browser = Browser.showPlugin mo.browser
                          }
                        , eff
                        )

                    PluginInterface.UpdateAddresses id pmsg ->
                        ( { mo
                            | layers = Layer.updateAddresses id (Plugin.updateAddress plugins pmsg) mo.layers
                          }
                            |> refreshBrowserAddress id
                        , eff
                        )

                    PluginInterface.UpdateAddressesByRootAddress _ _ ->
                        n model

                    PluginInterface.UpdateAddressesByEntityPathfinder _ _ ->
                        n model

                    PluginInterface.UpdateAddressEntities id pmsg ->
                        let
                            entityIds =
                                Layer.getAddresses id mo.layers
                                    |> List.map .entityId
                        in
                        ( entityIds
                            |> List.map (\i -> { currency = Id.currency i, entity = Id.entityId i })
                            |> List.foldl
                                refreshBrowserEntity
                                { mo
                                    | layers =
                                        entityIds
                                            |> List.foldl
                                                (\i -> Layer.updateEntity i (Plugin.updateEntity plugins pmsg))
                                                mo.layers
                                }
                        , eff
                        )

                    PluginInterface.UpdateEntities id pmsg ->
                        ( { mo
                            | layers = Layer.updateEntities id (Plugin.updateEntity plugins pmsg) mo.layers
                          }
                            |> refreshBrowserEntity id
                        , eff
                        )

                    PluginInterface.UpdateEntitiesByRootAddress id pmsg ->
                        let
                            predicate =
                                \{ entity } ->
                                    entity.rootAddress
                                        == id.address
                                        && entity.currency
                                        == id.currency
                        in
                        ( { mo
                            | layers =
                                Layer.updateEntitiesIf
                                    predicate
                                    (Plugin.updateEntity plugins pmsg)
                                    mo.layers
                          }
                            |> refreshBrowserEntityIf predicate
                        , eff
                        )

                    PluginInterface.LoadAddressIntoGraph address ->
                        model
                            |> loadAddress plugins
                                { currency = address.currency
                                , address = address.address
                                , table = Nothing
                                , at = Nothing
                                }

                    PluginInterface.GetEntitiesForAddresses _ _ ->
                        ( mo, [] )

                    PluginInterface.GetEntities _ _ ->
                        ( mo, [] )

                    PluginInterface.PushUrl _ ->
                        ( mo, [] )

                    PluginInterface.GetSerialized _ ->
                        ( mo, [] )

                    PluginInterface.OutMsgsPathfinder _ ->
                        ( mo, [] )

                    PluginInterface.Deserialize _ _ ->
                        ( mo, [] )

                    PluginInterface.GetAddressDomElement _ _ ->
                        ( mo, [] )

                    PluginInterface.SendToPort _ ->
                        ( mo, [] )

                    PluginInterface.ApiRequest _ ->
                        ( mo, [] )

                    PluginInterface.ShowDialog _ ->
                        ( mo, [] )

                    PluginInterface.CloseDialog ->
                        ( mo, [] )

                    PluginInterface.ShowNotification _ ->
                        ( mo, [] )

                    PluginInterface.OpenTooltip _ _ ->
                        ( mo, [] )

                    PluginInterface.CloseTooltip _ _ ->
                        ( mo, [] )
            )
            ( model, [] )


refreshBrowserAddress : A.Address -> Model -> Model
refreshBrowserAddress id model =
    { model
        | browser =
            case model.browser.type_ of
                Browser.Address (Loaded ad) table ->
                    if ad.address.currency == id.currency && ad.address.address == id.address then
                        model.browser
                            |> s_type_
                                (Layer.getAddress ad.id model.layers
                                    |> Maybe.map (\a -> Browser.Address (Loaded a) table)
                                    |> Maybe.withDefault model.browser.type_
                                )

                    else
                        model.browser

                _ ->
                    model.browser
    }


refreshBrowserEntity : E.Entity -> Model -> Model
refreshBrowserEntity id model =
    refreshBrowserEntityIf
        (\en -> en.entity.currency == id.currency && en.entity.entity == id.entity)
        model


refreshBrowserEntityIf : (Entity -> Bool) -> Model -> Model
refreshBrowserEntityIf predicate model =
    { model
        | browser =
            case model.browser.type_ of
                Browser.Entity (Loaded en) table ->
                    if predicate en then
                        model.browser
                            |> s_type_
                                (Layer.getEntity en.id model.layers
                                    |> Maybe.map (\e -> Browser.Entity (Loaded e) table)
                                    |> Maybe.withDefault model.browser.type_
                                )

                    else
                        model.browser

                _ ->
                    model.browser
    }


addUserTag : Set Id.AddressId -> Dict ( String, String, String ) Tag.UserTag -> IntDict Layer -> IntDict Layer
addUserTag ids userTags layers =
    ids
        |> Set.foldl
            (\id layers_ ->
                Dict.get ( Id.currency id, Id.addressId id, "address" ) userTags
                    |> Maybe.Extra.orElseLazy
                        (\_ ->
                            Dict.get ( Id.currency id, Id.addressId id, "entity" ) userTags
                        )
                    |> Maybe.map (\tag -> Layer.updateAddress id (\a -> { a | userTag = Just tag }) layers_)
                    |> Maybe.withDefault layers_
            )
            layers


makeLegend : Update.Config -> Model -> Tool.Toolbox
makeLegend uc model =
    let
        getCategories a =
            [ a.category
            , a.userTag |> Maybe.andThen .category
            ]
                |> List.filterMap identity
    in
    (Layer.addresses model.layers
        |> List.concatMap getCategories
    )
        ++ (Layer.entities model.layers |> List.concatMap getCategories)
        |> Set.fromList
        |> Set.toList
        |> List.filterMap
            (\cat ->
                List.Extra.find (.id >> (==) cat) uc.allConcepts
                    |> Maybe.map
                        (\category ->
                            { color = uc.categoryToColor category.id
                            , title = category.label
                            , uri = category.uri
                            }
                        )
            )
        |> Tool.Legend


makeTagPack : Model -> Time.Posix -> String
makeTagPack model time =
    Yaml.Encode.record
        [ ( "title", Yaml.Encode.string "TagPack exported from Iknaio Dashboard" )
        , ( "creator", Yaml.Encode.string "tbd" )
        , ( "tags"
          , model.userAddressTags
                |> Dict.values
                |> Yaml.Encode.list
                    (\{ currency, address, label, category, abuse, isClusterDefiner } ->
                        Yaml.Encode.record
                            [ ( "currency", Yaml.Encode.string currency )
                            , ( "address", Yaml.Encode.string address )
                            , ( "is_cluster_definer", Yaml.Encode.bool isClusterDefiner )
                            , ( "label"
                              , Json.Encode.string label
                                    |> Json.Encode.encode 0
                                    |> Yaml.Encode.string
                              )
                            , ( "category"
                              , category
                                    |> Maybe.map Yaml.Encode.string
                                    |> Maybe.withDefault Yaml.Encode.null
                              )
                            , ( "abuse"
                              , abuse
                                    |> Maybe.map Yaml.Encode.string
                                    |> Maybe.withDefault Yaml.Encode.null
                              )
                            , ( "lastmod"
                              , DateFormat.format
                                    [ DateFormat.yearNumber
                                    , DateFormat.text "-"
                                    , DateFormat.monthFixed
                                    , DateFormat.text "-"
                                    , DateFormat.dayOfMonthFixed
                                    , DateFormat.text " "
                                    , DateFormat.hourMilitaryFixed
                                    , DateFormat.text ":"
                                    , DateFormat.minuteFixed
                                    , DateFormat.text ":"
                                    , DateFormat.secondFixed
                                    ]
                                    Time.utc
                                    time
                                    |> (\s -> "\"" ++ s ++ "\"")
                                    |> Yaml.Encode.string
                              )
                            ]
                    )
          )
        ]
        |> Yaml.Encode.toString 2


tagId : A.Address -> Tag.UserTag -> String
tagId { currency, address } { label, source } =
    [ address
    , currency
    , label
    , source
    ]
        |> String.join "|"


importTagPack : Update.Config -> List Tag.UserTag -> Model -> Model
importTagPack uc tags model =
    tags
        |> List.foldl
            (storeUserTag uc)
            model



-- Helper function to check which tags can be applied to existing addresses


checkTagsCanBeApplied : List Tag.UserTag -> Model -> { totalTags : Int, applicableTags : Int }
checkTagsCanBeApplied tags model =
    let
        applicableCount =
            tags
                |> List.filter
                    (\tag ->
                        Layer.getFirstAddress { currency = tag.currency, address = tag.address } model.layers
                            /= Nothing
                    )
                |> List.length
    in
    { totalTags = List.length tags
    , applicableTags = applicableCount
    }


decodeYamlTag : Yaml.Decode.Decoder Tag.UserTag
decodeYamlTag =
    let
        optionalFieldWithDefault default name decoder =
            Yaml.Decode.oneOf
                [ Yaml.Decode.field name decoder
                , Yaml.Decode.succeed default
                ]

        optionalField name decoder =
            Yaml.Decode.oneOf
                [ Yaml.Decode.field name (Yaml.Decode.nullable decoder)
                , Yaml.Decode.succeed Nothing
                ]
    in
    Yaml.Decode.map7 Tag.UserTag
        (Yaml.Decode.field "currency" Yaml.Decode.string)
        (Yaml.Decode.field "address" Yaml.Decode.string)
        (Yaml.Decode.field "label" Yaml.Decode.string)
        (optionalFieldWithDefault "" "source" Yaml.Decode.string)
        (optionalField "category" Yaml.Decode.string)
        (optionalField "abuse" Yaml.Decode.string)
        (optionalFieldWithDefault False "is_cluster_definer" Yaml.Decode.bool)


deserialize : Json.Decode.Value -> Result Json.Decode.Error Deserialized
deserialize =
    Json.Decode.oneOf
        [ Json.Decode.index 0 Json.Decode.string
            |> Json.Decode.andThen deserializeByVersion
        , Json.Decode.null
            { addresses = []
            , entities = []
            , highlights = []
            }
        ]
        |> Json.Decode.decodeValue


deserializeByVersion : String -> Json.Decode.Decoder Deserialized
deserializeByVersion version =
    if String.startsWith "0.4.4" version then
        Graph044.decoder

    else if String.startsWith "0.4.5" version then
        Graph045.decoder

    else if String.startsWith "0.5." version then
        Graph050.decoder

    else if String.startsWith "1.0." version then
        Graph100.decoder

    else
        Json.Decode.fail ("unknown version " ++ version)


pushHistory : Msg -> Model -> Model
pushHistory msg model =
    if History.shallPushHistory msg model then
        forcePushHistory model

    else
        model


forcePushHistory : Model -> Model
forcePushHistory model =
    { model
        | history =
            makeHistoryEntry model
                |> History.push model.history
    }


cleanHistory : ( Model, List Effect ) -> ( Model, List Effect )
cleanHistory ( model, eff ) =
    ( { model
        | history =
            makeHistoryEntry model
                |> History.prune model.history
      }
    , eff
    )


fromDeserialized : Deserialized -> Model -> ( Model, List Effect )
fromDeserialized deserialized model =
    let
        unique =
            deserialized.addresses
                |> List.map .id
                |> List.map (\id -> ( Id.currency id, Id.addressId id ))
                |> Set.fromList
                |> Set.toList
                |> List.Extra.gatherEqualsBy first
                |> List.map (\( fst, more ) -> ( first fst, second fst :: List.map second more ))

        -- Create a layer offset for each currency to prevent layer conflicts
        currencyOffsets =
            unique
                |> List.indexedMap (\index ( currency, _ ) -> ( currency, index * 1000 ))
                |> Dict.fromList

        -- Apply offsets to make layer numbers globally unique across currencies
        offsetDeserialized currency deserializedData =
            let
                offset =
                    Dict.get currency currencyOffsets |> Maybe.withDefault 0
            in
            { deserializedData
                | addresses =
                    deserializedData.addresses
                        |> List.filter (\addr -> Id.currency addr.id == currency)
                        |> List.map
                            (\addr ->
                                { addr | id = ( Id.layer addr.id + offset, Id.currency addr.id, Id.addressId addr.id ) }
                            )
                , entities =
                    deserializedData.entities
                        |> List.filter (\ent -> Id.currency ent.id == currency)
                        |> List.map
                            (\ent ->
                                { ent | id = ( Id.layer ent.id + offset, Id.currency ent.id, Id.entityId ent.id ) }
                            )
            }
    in
    unique
        |> List.map
            (\( currency, addrs ) ->
                { deserialized = offsetDeserialized currency deserialized
                , addresses = []
                , entities = []
                }
                    |> BrowserGotBulkAddresses currency
                    |> BulkGetAddressEffect
                        { currency = currency
                        , addresses = addrs
                        }
                    |> ApiEffect
            )
        |> pair
            { model
                | highlights =
                    Highlighter.init
                        |> s_highlights deserialized.highlights
                , history = History.init
                , layers = IntDict.empty
                , config =
                    model.config
                        |> s_highlighter False
            }


addAddressesAtEntity : Plugins -> Update.Config -> EntityId -> List Api.Data.Address -> Model -> ( Model, List Effect )
addAddressesAtEntity plugins uc entityId addresses model =
    let
        added =
            List.foldl
                (Layer.addAddressAtEntity plugins uc entityId)
                { layers = model.layers
                , new = Set.empty
                , repositioned = Set.empty
                }
                addresses
    in
    ( { model
        | layers =
            added.layers
                |> addUserTag added.new model.userAddressTags
      }
        |> syncLinks added.repositioned
    , addresses
        |> List.map
            (\address ->
                BrowserGotAddressTags
                    { currency = address.currency
                    , address = address.address
                    }
                    |> GetAddressTagsEffect
                        { currency = address.currency
                        , address = address.address
                        , pagesize = 10
                        , nextpage = Nothing
                        , includeBestClusterTag = False
                        }
                    |> ApiEffect
            )
    )
        |> mapSecond
            ((++)
                (added.new
                    |> Set.toList
                    |> List.concatMap
                        (\a ->
                            getAddressEgonet a BrowserGotAddressEgonet added.layers
                                |> List.map ApiEffect
                        )
                )
            )
        |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))


loadAddressPath :
    Plugins
    ->
        { currency : String
        , addresses : List String
        }
    -> Model
    -> ( Model, List Effect )
loadAddressPath plugins { currency, addresses } model =
    case addresses of
        address :: rest ->
            { model
                | adding = Adding.setAddressPath currency address rest model.adding
            }
                |> loadAddress
                    plugins
                    { currency = currency
                    , address = address
                    , table = Nothing
                    , at = Nothing
                    }

        [] ->
            n model


loadEntityPath :
    Plugins
    ->
        { currency : String
        , entities : List Int
        }
    -> Model
    -> ( Model, List Effect )
loadEntityPath plugins { currency, entities } model =
    case entities of
        entity :: rest ->
            { model
                | adding = Adding.setEntityPath currency entity rest model.adding
            }
                |> loadEntity
                    plugins
                    { currency = currency
                    , entity = entity
                    , table = Nothing
                    , layer = Nothing
                    }

        [] ->
            n model


type At
    = AtLayer Int
    | AtAnchor Bool AddressId


loadAddress :
    Plugins
    ->
        { currency : String
        , address : String
        , table : Maybe Route.AddressTable
        , at : Maybe At
        }
    -> Model
    -> ( Model, List Effect )
loadAddress _ { currency, address, table, at } model =
    at
        |> Maybe.andThen
            (\l ->
                let
                    layer =
                        case l of
                            AtLayer ll ->
                                ll

                            AtAnchor isOutgoing id ->
                                Id.layer id
                                    |> layerDelta isOutgoing
                in
                Layer.getAddress (Id.initAddressId { currency = currency, id = address, layer = layer }) model.layers
            )
        |> Maybe.Extra.orElseLazy
            (\_ ->
                case at of
                    Nothing ->
                        Layer.getFirstAddress { currency = currency, address = address } model.layers

                    _ ->
                        Nothing
            )
        |> Maybe.map
            (\a ->
                selectAddress a table model
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                let
                    select =
                        model.selectIfLoaded == Just (SelectAddress { currency = currency, address = address })

                    browser =
                        if select then
                            Browser.loadingAddress { currency = currency, address = address } model.browser

                        else
                            model.browser

                    ( browser2, effects ) =
                        if select then
                            table
                                |> Maybe.map (\t -> Browser.showAddressTable t browser)
                                |> Maybe.withDefault (Browser.hideTable browser |> n)

                        else
                            n browser

                    anchor =
                        case at of
                            Just (AtAnchor isOutgoing a) ->
                                Just ( isOutgoing, a )

                            _ ->
                                Nothing
                in
                ( { model
                    | adding = Adding.loadAddress { currency = currency, address = address } anchor model.adding
                    , browser = browser2
                  }
                , [ BrowserGotEntityForAddress address
                        |> GetEntityForAddressEffect
                            { address = address
                            , currency = currency
                            }
                        |> ApiEffect
                  , BrowserGotAddress
                        |> GetAddressEffect
                            { address = address
                            , currency = currency
                            , includeActors = True
                            }
                        |> ApiEffect
                  ]
                    ++ effects
                )
            )


loadEntity :
    Plugins
    ->
        { currency : String
        , entity : Int
        , table : Maybe Route.EntityTable
        , layer : Maybe Int
        }
    -> Model
    -> ( Model, List Effect )
loadEntity _ { currency, entity, table, layer } model =
    layer
        |> Maybe.andThen
            (\l -> Layer.getEntity (Id.initEntityId { currency = currency, id = entity, layer = l }) model.layers)
        |> Maybe.Extra.orElseLazy
            (\_ -> Layer.getFirstEntity { currency = currency, entity = entity } model.layers)
        |> Maybe.map
            (\e ->
                selectEntity e table model
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                let
                    browser =
                        Browser.loadingEntity { currency = currency, entity = entity } model.browser

                    ( browser2, effects ) =
                        table
                            |> Maybe.map (\t -> Browser.showEntityTable t browser)
                            |> Maybe.withDefault (Browser.hideTable browser |> n)
                in
                ( { model
                    | browser = browser2
                    , adding = Adding.loadEntity { currency = currency, entity = entity } model.adding
                  }
                , (BrowserGotEntity
                    |> GetEntityEffect
                        { entity = entity
                        , currency = currency
                        }
                    |> ApiEffect
                  )
                    :: (getEntityEgonet
                            { currency = currency
                            , entity = entity
                            }
                            BrowserGotEntityEgonet
                            model.layers
                            |> List.map ApiEffect
                       )
                    ++ effects
                )
            )


normalizeDeserializedEntityTag : List Api.Data.Entity -> DeserializedEntityTag -> Maybe Tag.UserTag
normalizeDeserializedEntityTag entities entityTag =
    case entityTag of
        TagUserTag tag ->
            Just tag

        DeserializedEntityUserTagTag tag ->
            List.Extra.find (.entity >> (==) tag.entity) entities
                |> Maybe.map
                    (\entity ->
                        { currency = tag.currency
                        , address = entity.rootAddress
                        , label = tag.label
                        , source = tag.source
                        , category = tag.category
                        , abuse = tag.abuse
                        , isClusterDefiner = True
                        }
                    )


deselectHighlighter : Model -> Model
deselectHighlighter model =
    let
        highlights =
            Highlighter.deselect model.highlights
    in
    { model
        | highlights = highlights
        , config =
            model.config
                |> s_highlighter False
    }


tagInputToUserTag : Model -> Tag.Input -> Maybe Tag.UserTag
tagInputToUserTag model input =
    let
        ( currency, address, isClusterDefiner ) =
            case input.id of
                Node.Address a ->
                    ( Id.currency a
                    , Just (Id.addressId a)
                    , False
                    )

                Node.Entity a ->
                    ( Id.currency a
                    , Layer.getEntity a model.layers
                        |> Maybe.map (.entity >> .rootAddress)
                    , True
                    )
    in
    address
        |> Maybe.map
            (\addr ->
                { label = Model.Search.query input.label
                , category =
                    if String.isEmpty input.category then
                        Nothing

                    else
                        Just input.category
                , abuse =
                    if String.isEmpty input.abuse then
                        Nothing

                    else
                        Just input.abuse
                , source = input.source
                , currency = currency
                , address = addr
                , isClusterDefiner = isClusterDefiner
                }
            )


handleNotFound : Model -> Model
handleNotFound model =
    { model
        | browser =
            model.browser
                |> s_visible False
    }


selectAddressLinkIfLoaded : Model -> ( Model, List Effect )
selectAddressLinkIfLoaded model =
    case model.selectIfLoaded of
        Just (SelectAddresslink table src dst) ->
            Layer.getFirstAddress src model.layers
                |> Maybe.andThen
                    (\source ->
                        (case source.links of
                            Address.Links links ->
                                let
                                    t =
                                        Id.initAddressId
                                            { id = dst.address
                                            , currency = dst.currency
                                            , layer = Id.layer source.id + 1
                                            }
                                in
                                Dict.get t links
                        )
                            |> Maybe.map
                                (\link ->
                                    model
                                        |> s_selectIfLoaded Nothing
                                        |> selectAddressLink table source link
                                )
                    )
                |> Maybe.withDefault (n model)

        _ ->
            n model


selectAddressLink : Maybe Route.AddresslinkTable -> Address -> Link Address -> Model -> ( Model, List Effect )
selectAddressLink table source link model =
    let
        browser =
            Browser.showAddresslink
                { source = source
                , link = link
                }
                model.browser

        ( browser2, effects ) =
            table
                |> Maybe.map (\tb -> Browser.showAddresslinkTable tb browser)
                |> Maybe.withDefault (Browser.hideTable browser |> n)

        linkId =
            ( source.id, link.node.id )

        newmodel =
            deselect model
    in
    ( { newmodel
        | browser = browser2
        , selected = SelectedAddresslink linkId
        , layers = Layer.selectAddressLink linkId newmodel.layers
      }
    , effects
    )


selectEntityLinkIfLoaded : Model -> ( Model, List Effect )
selectEntityLinkIfLoaded model =
    case model.selectIfLoaded of
        Just (SelectEntitylink table src dst) ->
            Layer.getFirstEntity src model.layers
                |> Maybe.andThen
                    (\source ->
                        (case source.links of
                            Entity.Links links ->
                                let
                                    t =
                                        Id.initEntityId
                                            { id = dst.entity
                                            , currency = dst.currency
                                            , layer = Id.layer source.id + 1
                                            }
                                in
                                Dict.get t links
                        )
                            |> Maybe.map
                                (\link ->
                                    model
                                        |> s_selectIfLoaded Nothing
                                        |> selectEntityLink table source link
                                )
                    )
                |> Maybe.withDefault (n model)

        _ ->
            n model


selectEntityLink : Maybe Route.AddresslinkTable -> Entity -> Link Entity -> Model -> ( Model, List Effect )
selectEntityLink table source link model =
    let
        browser =
            Browser.showEntitylink
                { source = source
                , link = link
                }
                model.browser

        ( browser2, effects ) =
            table
                |> Maybe.map (\tb -> Browser.showEntitylinkTable tb browser)
                |> Maybe.withDefault (Browser.hideTable browser |> n)

        linkId =
            ( source.id, link.node.id )

        newmodel =
            deselect model
    in
    ( { newmodel
        | browser = browser2
        , selected = SelectedEntitylink ( source.id, link.node.id )
        , layers = Layer.selectEntityLink linkId newmodel.layers
      }
    , effects
    )


layerDelta : Bool -> Int -> Int
layerDelta isOutgoing =
    (+)
        (if isOutgoing then
            1

         else
            -1
        )


syncSelection : Model -> Model
syncSelection model =
    case model.selected of
        SelectedAddress id ->
            Layer.getAddress id model.layers
                |> Maybe.map
                    (\_ -> { model | layers = Layer.selectAddress id model.layers })
                |> Maybe.withDefault (deselect model)

        SelectedEntity id ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\_ -> { model | layers = Layer.selectEntity id model.layers })
                |> Maybe.withDefault (deselect model)

        SelectedAddresslink id ->
            Layer.getAddressLink id model.layers
                |> Maybe.map
                    (\_ -> { model | layers = Layer.selectAddressLink id model.layers })
                |> Maybe.withDefault (deselect model)

        SelectedEntitylink id ->
            Layer.getEntityLink id model.layers
                |> Maybe.map
                    (\_ -> { model | layers = Layer.selectEntityLink id model.layers })
                |> Maybe.withDefault (deselect model)

        SelectedNone ->
            model


updateTransformByBoundingBox : Update.Config -> Model -> Coords.BBox -> Model
updateTransformByBoundingBox uc model bbox =
    { model
        | transform =
            uc.size
                |> Maybe.map
                    (\{ width, height } ->
                        { width = width
                        , height = height
                        }
                    )
                |> Maybe.map
                    (\viewport ->
                        Transform.updateByBoundingBox viewport (Coords.addMargin bbox) model.transform
                    )
                |> Maybe.withDefault model.transform
    }


extendTransformWithBoundingBox : Update.Config -> Model -> Coords.BBox -> Model
extendTransformWithBoundingBox uc model bbox =
    { model
        | transform =
            uc.size
                |> Maybe.map
                    (\{ width, height } ->
                        { width = width
                        , height = height
                        }
                    )
                |> Maybe.map
                    (\viewport ->
                        Transform.getBoundingBox model.transform viewport
                            |> Coords.mergeBoundingBoxes (Coords.addMargin bbox)
                            |> flip (Transform.updateByBoundingBox viewport) model.transform
                    )
                |> Maybe.withDefault model.transform
    }


makeHistoryEntry : Model -> Entry.Model
makeHistoryEntry model =
    { layers = deselectLayers model.selected model.layers
    , highlights = model.highlights.highlights
    }


undoRedo : (History.Model Entry.Model -> Entry.Model -> Maybe ( History.Model Entry.Model, Entry.Model )) -> Model -> ( Model, List Effect )
undoRedo fun model =
    makeHistoryEntry model
        |> fun model.history
        |> Maybe.map
            (\( history, entry ) ->
                { model
                    | history = history
                    , layers = entry.layers
                    , highlights =
                        model.highlights
                            |> s_highlights entry.highlights
                }
                    |> syncSelection
            )
        |> Maybe.withDefault model
        |> n


repositionHovercards : Model -> ( Model, List Effect )
repositionHovercards model =
    [ repositionHovercardCmd model .tag TagHovercardMsg
    , repositionHovercardCmd model .search SearchHovercardMsg
    ]
        |> List.map CmdEffect
        |> pair model


repositionHovercardCmd : Model -> (Model -> Maybe { a | hovercard : Hovercard.Model }) -> (Hovercard.Msg -> Msg) -> Cmd Msg
repositionHovercardCmd model field toMsg =
    field model
        |> Maybe.map
            (.hovercard
                >> Hovercard.getElement
                >> Cmd.map toMsg
            )
        |> Maybe.withDefault Cmd.none
