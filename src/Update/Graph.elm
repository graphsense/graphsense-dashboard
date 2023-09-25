module Update.Graph exposing (..)

import Api.Data
import Browser.Dom as Dom
import Color
import Config.Graph exposing (maxExpandableAddresses, maxExpandableNeighbors)
import Config.Update as Update
import DateFormat
import Decode.Graph044 as Graph044
import Decode.Graph045 as Graph045
import Decode.Graph050 as Graph050
import Decode.Graph100 as Graph100
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Api exposing (Effect(..), getAddressEgonet, getEntityEgonet)
import Effect.Graph exposing (Effect(..))
import Encode.Graph as Encode
import File
import File.Select
import Init.Graph.ContextMenu as ContextMenu
import Init.Graph.Highlighter as Highlighter
import Init.Graph.Id as Id
import Init.Graph.Search as Search
import Init.Graph.Tag as Tag
import IntDict exposing (IntDict)
import Json.Decode
import Json.Encode exposing (Value)
import List.Extra
import Log
import Maybe.Extra
import Model.Address as A
import Model.Entity as E
import Model.Graph exposing (..)
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Browser as Browser
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Highlighter as Highlighter
import Model.Graph.Id as Id exposing (AddressId, EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Link as Link exposing (Link)
import Model.Graph.Search as Search
import Model.Graph.Tag as Tag
import Model.Graph.Tool as Tool
import Model.Node as Node
import Msg.Graph as Msg exposing (Msg(..))
import Msg.Search as Search
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
import Update.Graph.Adding as Adding
import Update.Graph.Address as Address
import Update.Graph.Browser as Browser
import Update.Graph.Color as Color
import Update.Graph.Entity as Entity
import Update.Graph.Highlighter as Highlighter
import Update.Graph.History as History
import Update.Graph.Layer as Layer
import Update.Graph.Search as Search
import Update.Graph.Tag as Tag
import Update.Graph.Transform as Transform
import Yaml.Decode
import Yaml.Encode


maxHistory : Int
maxHistory =
    10


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
            Layer.addAddress plugins uc newModel.config.colors address newModel.layers

        newModel_ =
            { newModel
                | layers =
                    added.layers
                        |> addUserTag added.new model.userAddressTags
                , config =
                    newModel.config
                        |> s_colors added.colors
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
                    }
                |> ApiEffect
    in
    addedAddress
        |> Maybe.map
            (\a ->
                selectAddress a Nothing newModel_
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
                            Layer.addEntity plugins uc model.config.colors entity model.layers

                        else
                            Layer.addEntitiesAt plugins
                                uc
                                (Layer.anchorsToPositions (Just outgoingAnchors) model.layers)
                                [ entity ]
                                { layers = model.layers
                                , new = Set.empty
                                , colors = model.config.colors
                                , repositioned = Set.empty
                                }
                                |> Layer.addEntitiesAt plugins
                                    uc
                                    (Layer.anchorsToPositions (Just incomingAnchors) model.layers)
                                    [ entity ]

                    newModel =
                        { model
                            | layers = added.layers
                            , config =
                                model.config
                                    |> s_colors added.colors
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
        |> cleanHistory
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
loadNextAddress plugins uc model id =
    let
        items_out_of_bbox =
            not (uc.size |> Maybe.map (Layer.isContentWithinViewPort model.layers model.transform) |> Maybe.withDefault True)

        add_fitgraph_on_path =
            if Adding.isLastPathItem model.adding && items_out_of_bbox then
                CmdEffect (Task.succeed Msg.UserClickedFitGraph |> Task.perform (\x -> x)) |> List.singleton

            else
                []
    in
    Adding.getNextFor id model.adding
        |> Maybe.map
            (\nextId ->
                { model
                    | adding = Adding.popPath model.adding
                }
                    |> s_selectIfLoaded (Just (SelectAddress (A.fromId nextId)))
                    |> loadAddress
                        plugins
                        { currency = Id.currency nextId
                        , address = Id.addressId nextId
                        , table = Nothing
                        , at = AtAnchor True id |> Just
                        }
                    |> (\( m, eff ) -> ( m, eff ++ add_fitgraph_on_path ))
            )
        |> Maybe.withDefault ( model, add_fitgraph_on_path )


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

        InternalGraphAddedEntities ids ->
            n model

        InternalGraphSelectedAddress id ->
            loadNextAddress plugins uc model id

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
                model.tag
                    |> Maybe.map
                        (\tag ->
                            Tag.searchMsg Search.UserLeavesSearch tag
                                |> mapFirst (\t -> { model | tag = Just t })
                        )
                    |> Maybe.withDefault
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
                        , dragging = DraggingNode id start coords
                    }
                        |> syncLinks (Set.singleton id)
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

        UserPressesEscape ->
            deselectHighlighter model |> n

        UserClickedAddress id ->
            case Highlighter.getSelectedColor model.highlights of
                Nothing ->
                    Route.addressRoute
                        { currency = Id.currency id
                        , address = Id.addressId id
                        , table = Nothing
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
                            , table = Nothing
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
                , table = Nothing
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
                , table = Nothing
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
                |> n

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
                            |> List.map ApiEffect
                        )
                    )

        BrowserGotEntityNeighborsTable id isOutgoing neighbors ->
            { model
                | browser = Browser.showEntityNeighbors id isOutgoing neighbors model.browser
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
            let
                colors =
                    tags.addressTags
                        |> List.map .category
                        |> List.foldl
                            (\category config -> Color.update uc config category)
                            model.config.colors
            in
            { model
                | config =
                    model.config |> s_colors colors
            }
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
                    if String.toLower id.currency == "eth" then
                        Browser.showAddressTxsAccount id data model.browser

                    else
                        Browser.showAddressTxsUtxo id data model.browser
            }
                |> n

        BrowserGotAddresslinkTxs id data ->
            { model
                | browser =
                    if String.toLower id.currency == "eth" then
                        Browser.showAddresslinkTxsAccount id data model.browser

                    else
                        Browser.showAddresslinkTxsUtxo id data model.browser
            }
                |> n

        BrowserGotEntityTxs id data ->
            { model
                | browser =
                    if String.toLower id.currency == "eth" then
                        Browser.showEntityTxsAccount id data model.browser

                    else
                        Browser.showEntityTxsUtxo id data model.browser
            }
                |> n

        BrowserGotEntitylinkTxs id data ->
            { model
                | browser =
                    if String.toLower id.currency == "eth" then
                        Browser.showEntitylinkTxsAccount id data model.browser

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
                    if String.toLower id.currency == "eth" then
                        Browser.showBlockTxsAccount id data model.browser

                    else
                        Browser.showBlockTxsUtxo id data model.browser
            }
                |> n

        BrowserGotTokenTxs id data ->
            { model
                | browser =
                    Browser.showTokenTxs id data model.browser
            }
                |> n

        TableNewState state ->
            { model
                | browser = Browser.tableNewState state model.browser
            }
                |> n

        PluginMsg msgValue ->
            -- handled in src/Update.elm
            n model

        UserClickedContextMenu ->
            hideContextmenu model

        UserLeftContextMenu ->
            hideContextmenu model

        UserClickedAnnotateAddress id ->
            ( model
            , Id.addressIdToString id
                |> Dom.getElement
                |> Task.attempt (BrowserGotAddressElementForAnnotate id)
                |> CmdEffect
                |> List.singleton
            )

        BrowserGotAddressElementForAnnotate id element ->
            element
                |> Result.map
                    (\el ->
                        { model
                            | tag =
                                model.layers
                                    |> Layer.getAddress id
                                    |> Maybe.andThen .userTag
                                    |> Tag.initAddressTag id el
                                    |> Just
                        }
                    )
                |> Result.withDefault model
                |> n

        UserClickedAnnotateEntity id ->
            ( model
            , Id.entityIdToString id
                |> Dom.getElement
                |> Task.attempt (BrowserGotEntityElementForAnnotate id)
                |> CmdEffect
                |> List.singleton
            )

        BrowserGotEntityElementForAnnotate id element ->
            element
                |> Result.map
                    (\el ->
                        { model
                            | tag =
                                model.layers
                                    |> Layer.getEntity id
                                    |> Maybe.andThen .userTag
                                    |> Tag.initEntityTag id el
                                    |> Just
                        }
                    )
                |> Result.withDefault model
                |> n

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
                |> Maybe.map (\tag -> deleteUserTag tag model)
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
                        Browser.Entity (Browser.Loaded e) _ ->
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
                        { colors = model.config.colors
                        , layers = model.layers
                        , new = Set.empty
                        , repositioned = Set.empty
                        }
            in
            ( { model
                | layers =
                    added.layers
                        |> addUserTag added.new model.userAddressTags
                , config = model.config |> s_colors added.colors
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
                                                , colors = model.config.colors
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
                                                            , config = model.config |> s_colors added.colors
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
            makeLegend model
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
                , layers =
                    Highlighter.getSelectedColor model.highlights
                        |> Maybe.map
                            (\before ->
                                Layer.updateEntityColor before (Just color) model.layers
                                    |> Layer.updateAddressColor before (Just color)
                            )
                        |> Maybe.withDefault model.layers
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

        UserChangesCurrency currency ->
            -- handled upstream
            n model

        UserChangesValueDetail detail ->
            -- handled upstream
            n model

        UserChangesAddressLabelType at ->
            { model
                | config =
                    model.config
                        |> s_addressLabelType
                            (case at of
                                "id" ->
                                    Config.Graph.ID

                                "balance" ->
                                    Config.Graph.Balance

                                "tag" ->
                                    Config.Graph.Tag

                                _ ->
                                    model.config.addressLabelType
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
            ( model
            , Id.entityIdToString id
                |> Dom.getElement
                |> Task.attempt (BrowserGotEntityElementForSearch id)
                |> CmdEffect
                |> List.singleton
            )

        BrowserGotEntityElementForSearch id result ->
            result
                |> Result.map
                    (\element ->
                        { model
                            | search = Search.init model.config.entityConcepts element id |> Just
                        }
                    )
                |> Result.withDefault model
                |> n

        UserSelectsDirection direction ->
            updateSearch (Search.selectDirection direction) model

        UserSelectsCriterion criterion ->
            updateSearch
                (Search.selectCriterion
                    { categories = model.config.entityConcepts
                    }
                    criterion
                )
                model

        UserSelectsSearchCategory category ->
            updateSearch (Search.selectCategory category) model

        UserInputsSearchDepth input ->
            input
                |> Maybe.map
                    (\depth ->
                        updateSearch (\s -> n { s | depth = depth }) model
                    )
                |> Maybe.withDefault (n model)

        UserInputsSearchBreadth input ->
            input
                |> Maybe.map
                    (\breadth ->
                        updateSearch (\s -> n { s | breadth = breadth }) model
                    )
                |> Maybe.withDefault (n model)

        UserInputsSearchMaxAddresses input ->
            input
                |> Maybe.map
                    (\maxAddresses ->
                        updateSearch (\s -> n { s | maxAddresses = maxAddresses }) model
                    )
                |> Maybe.withDefault (n model)

        UserSubmitsSearchInput ->
            updateSearch Search.submit model
                |> mapFirst (s_search Nothing)

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

        UserClickedExportGraphics time ->
            -- handled upstream
            n model

        UserClickedExportTagPack time ->
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

        BrowserReadTagPackFile filename result ->
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

        PortDeserializedGS data ->
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
                                            acc.layers
                                    )
                        , config =
                            model.config
                                |> s_colors acc.colors
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
                        , onlyIds = True
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

        BrowserGotBulkAddressTags currency tags ->
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
            case model.history of
                History (recent :: rest) future ->
                    { model
                        | layers = recent
                        , history =
                            deselectLayers model.selected model.layers
                                :: future
                                |> History rest
                    }
                        |> deselect
                        |> n

                _ ->
                    n model

        UserClickedRedo ->
            case model.history of
                History past (recent :: future) ->
                    { model
                        | layers = recent
                        , history =
                            History (model.layers :: past) future
                    }
                        |> n

                _ ->
                    n model

        UserClickedNew ->
            -- handled upstream
            n model

        UserClickedNewYes ->
            -- handled upstream
            n model

        UserInputsFilterTable input ->
            ( { model
                | browser = Browser.filterTable input model.browser
              }
            , Dom.focus "tableFilter"
                |> Task.attempt (\_ -> NoOp)
                |> CmdEffect
                |> List.singleton
            )

        UserClickedFitGraph ->
            let
                marginX =
                    Config.Graph.entityWidth / 2

                marginY =
                    Config.Graph.entityMinHeight / 2

                addMargin bbox =
                    { x = bbox.x - marginX
                    , y = bbox.y - marginY
                    , width = bbox.width + marginX * 2
                    , height = bbox.height + marginY * 2
                    }
            in
            { model
                | transform =
                    Layer.getBoundingBox model.layers
                        |> Maybe.map addMargin
                        |> Maybe.map2
                            (Transform.updateByBoundingBox
                                model.transform
                            )
                            (uc.size
                                |> Maybe.map
                                    (\{ width, height } ->
                                        { width = width
                                        , height = height
                                        }
                                    )
                            )
                        |> Maybe.withDefault model.transform
            }
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
            { model
                | config =
                    model.config
                        |> s_showDatesInUserLocale (not model.config.showDatesInUserLocale)
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
            , Browser.tableAsCSV uc.locale model.config model.browser
                |> Maybe.map (DownloadCSVEffect >> List.singleton)
                |> Maybe.withDefault []
            )

        UserClickedExternalLink url ->
            ( model, Ports.newTab url |> CmdEffect |> List.singleton )

        UserClickedCopyToClipboard value ->
            ( model, Ports.copyToClipboard value |> CmdEffect |> List.singleton )

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

        Route.Currency currency (Route.AddressPath addresses) ->
            loadPath plugins
                { currency = currency
                , addresses = addresses
                }
                model

        Route.Currency currency (Route.Entity e table layer) ->
            layer
                |> Maybe.andThen
                    (\l -> Layer.getEntity (Id.initEntityId { currency = currency, id = e, layer = l }) model.layers)
                |> Maybe.Extra.orElseLazy
                    (\_ -> Layer.getFirstEntity { currency = currency, entity = e } model.layers)
                |> Maybe.map
                    (\entity ->
                        model
                            |> s_selectIfLoaded (Just (SelectEntity { currency = currency, entity = e }))
                            |> selectEntity entity table
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
                            , selectIfLoaded = Just (SelectEntity { currency = currency, entity = e })
                          }
                        , [ BrowserGotEntity
                                |> GetEntityEffect
                                    { entity = e
                                    , currency = currency
                                    }
                                |> ApiEffect
                          ]
                            ++ (getEntityEgonet
                                    { currency = currency
                                    , entity = e
                                    }
                                    BrowserGotEntityEgonet
                                    model.layers
                                    |> List.map ApiEffect
                               )
                            ++ effects
                        )
                    )

        Route.Currency currency (Route.Tx t table tokenTxId) ->
            let
                ( browser, effect ) =
                    if String.toLower currency == "eth" || tokenTxId /= Nothing then
                        Browser.loadingTxAccount { currency = currency, txHash = t, tokenTxId = tokenTxId } currency model.browser

                    else
                        Browser.loadingTxUtxo { currency = currency, txHash = t } model.browser

                ( browser2, effects ) =
                    if String.toLower currency == "eth" then
                        table
                            |> Maybe.map (\tb -> Browser.showTxAccountTable tb browser)
                            |> Maybe.withDefault (n browser)

                    else if tokenTxId == Nothing then
                        table
                            |> Maybe.map (\tb -> Browser.showTxUtxoTable tb browser)
                            |> Maybe.withDefault (n browser)

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
                browser =
                    Browser.loadingBlock { currency = currency, block = b } model.browser

                ( browser2, effects ) =
                    table
                        |> Maybe.map (\tb -> Browser.showBlockTable tb browser)
                        |> Maybe.withDefault (n browser)
            in
            ( { model
                | browser = browser2
              }
            , [ BrowserGotBlock
                    |> GetBlockEffect
                        { height = b
                        , currency = currency
                        }
                    |> ApiEffect
              ]
                ++ effects
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
                |> Maybe.withDefault (n model)

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
                |> Maybe.withDefault (n model)

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
                        Browser.Actor (Browser.Loading currentActorId _) _ ->
                            if currentActorId /= actorId then
                                ( Browser.loadingActor actorId model.browser, getActorAction )

                            else
                                ( Browser.openActor True model.browser, [] )

                        Browser.Actor (Browser.Loaded actor) _ ->
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

        Route.Plugin ( pid, value ) ->
            n model


addAddressNeighborsWithEntity : Plugins -> Update.Config -> ( Address, Entity ) -> Bool -> ( List Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> { model : Model, newAddresses : List Address, newEntities : List EntityId, repositioned : Set EntityId }
addAddressNeighborsWithEntity plugins uc ( anchorAddress, anchorEntity ) isOutgoing ( neighbors, entity ) model =
    let
        acc =
            Layer.addEntityNeighbors plugins uc anchorEntity isOutgoing model.config.colors [ entity ] model.layers
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
                                    , colors = added__.colors
                                    }
                                )
                                { layers = acc.layers
                                , colors = acc.colors
                                , new = Set.empty
                                , repositioned = acc.repositioned
                                }
                in
                { model =
                    { model
                        | layers = added.layers
                        , config =
                            model.config
                                |> s_colors added.colors
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
            Layer.addEntityNeighbors plugins uc anchor isOutgoing model.config.colors (List.map .entity neighbors) model.layers

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
            List.map
                (\neighbor ->
                    Layer.getEntities neighbor.entity.currency neighbor.entity.entity model.layers
                        |> List.map (pair neighbor)
                )
                neighbors
                |> List.concat

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
        |> List.map
            (\{ entity } ->
                getEntityEgonet
                    { currency = entity.currency
                    , entity = entity.entity
                    }
                    BrowserGotEntityEgonet
                    newModel.layers
                    |> List.map ApiEffect
            )
        |> List.concat
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
                        }
                    |> ApiEffect
            )
        |> (++)
            (added.newAddresses
                |> List.map .id
                |> List.map
                    (\a ->
                        getAddressEgonet a BrowserGotAddressEgonet added.model.layers
                            |> List.map ApiEffect
                    )
                |> List.concat
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
                    |> Maybe.withDefault (n browser1)

            newmodel =
                deselect model
        in
        ( { newmodel
            | browser = browser2
            , selected = SelectedAddress address.id
            , selectIfLoaded = Nothing
            , layers = Layer.updateAddress address.id (\a -> { a | selected = True }) newmodel.layers
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
                    |> Maybe.withDefault (n browser1)

            newmodel =
                deselect model
        in
        ( { newmodel
            | browser = browser2
            , selected = SelectedEntity entity.id
            , selectIfLoaded = Nothing
            , layers =
                Layer.updateEntity entity.id (\e -> { e | selected = True }) newmodel.layers
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
            colors =
                tag.category
                    |> Color.update uc model.config.colors

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
            , config =
                model.config
                    |> s_colors colors
            , tag = Nothing
            , browser = Browser.updateUserTags (Dict.values userAddressTags) model.browser
        }
            |> updateLegend
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


deleteUserTag : Tag.UserTag -> Model -> Model
deleteUserTag tag model =
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
        |> updateLegend
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


updateLegend : Model -> Model
updateLegend model =
    { model
        | activeTool =
            case model.activeTool.toolbox of
                Tool.Legend _ ->
                    model.activeTool
                        |> s_toolbox (makeLegend model)

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

                    PluginInterface.GetEntitiesForAddresses _ _ ->
                        ( mo, [] )

                    PluginInterface.GetEntities _ _ ->
                        ( mo, [] )

                    PluginInterface.PushUrl url ->
                        ( mo, [] )

                    PluginInterface.GetSerialized pmsg ->
                        ( mo, [] )

                    PluginInterface.Deserialize _ _ ->
                        ( mo, [] )

                    PluginInterface.GetAddressDomElement id pmsg ->
                        ( mo, [] )

                    PluginInterface.SendToPort _ ->
                        ( mo, [] )

                    PluginInterface.ApiRequest _ ->
                        ( mo, [] )

                    PluginInterface.ShowDialog _ ->
                        ( mo, [] )
            )
            ( model, [] )


refreshBrowserAddress : A.Address -> Model -> Model
refreshBrowserAddress id model =
    { model
        | browser =
            case model.browser.type_ of
                Browser.Address (Browser.Loaded ad) table ->
                    if ad.address.currency == id.currency && ad.address.address == id.address then
                        model.browser
                            |> s_type_
                                (Layer.getAddress ad.id model.layers
                                    |> Maybe.map (\a -> Browser.Address (Browser.Loaded a) table)
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
                Browser.Entity (Browser.Loaded en) table ->
                    if predicate en then
                        model.browser
                            |> s_type_
                                (Layer.getEntity en.id model.layers
                                    |> Maybe.map (\e -> Browser.Entity (Browser.Loaded e) table)
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
        |> Set.toList
        |> List.foldl
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


makeLegend : Model -> Tool.Toolbox
makeLegend model =
    model.config.colors
        |> Dict.toList
        |> List.filterMap
            (\( cat, color ) ->
                List.Extra.find (.id >> (==) cat) model.config.entityConcepts
                    |> Maybe.map
                        (\category ->
                            { color = color
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
                    (\{ currency, address, label, source, category, abuse, isClusterDefiner } ->
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
    if History.shallPushHistory msg then
        forcePushHistory model

    else
        model


forcePushHistory : Model -> Model
forcePushHistory model =
    case model.history of
        History past future ->
            { model
                | history =
                    History
                        (deselectLayers model.selected model.layers :: past)
                        []
            }


cleanHistory : ( Model, List Effect ) -> ( Model, List Effect )
cleanHistory ( model, eff ) =
    let
        filter old =
            case old of
                fst :: rest ->
                    if fst == model.layers then
                        filter rest

                    else
                        old

                [] ->
                    []
    in
    ( if List.isEmpty eff then
        case model.history of
            History past future ->
                { model | history = History (filter past |> List.take maxHistory) future }

      else
        model
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
    in
    unique
        |> List.map
            (\( currency, addrs ) ->
                { deserialized = deserialized
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
                , history = History [] []
                , layers = IntDict.empty
                , config =
                    model.config
                        |> s_highlighter False
            }


serialize : Model -> Value
serialize =
    Encode.encode


setEntityConcepts : List Api.Data.Concept -> Model -> Model
setEntityConcepts concepts model =
    { model
        | config =
            model.config
                |> s_entityConcepts concepts
    }


setAbuseConcepts : List Api.Data.Concept -> Model -> Model
setAbuseConcepts concepts model =
    { model
        | config =
            model.config
                |> s_abuseConcepts concepts
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
                , colors = model.config.colors
                }
                addresses
    in
    ( { model
        | layers =
            added.layers
                |> addUserTag added.new model.userAddressTags
        , config =
            model.config
                |> s_colors added.colors
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
                        }
                    |> ApiEffect
            )
    )
        |> mapSecond
            ((++)
                (added.new
                    |> Set.toList
                    |> List.map
                        (\a ->
                            getAddressEgonet a BrowserGotAddressEgonet added.layers
                                |> List.map ApiEffect
                        )
                    |> List.concat
                )
            )
        |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))


loadPath :
    Plugins
    ->
        { currency : String
        , addresses : List String
        }
    -> Model
    -> ( Model, List Effect )
loadPath plugins { currency, addresses } model =
    case addresses of
        address :: rest ->
            { model
                | adding = Adding.setPath currency address rest model.adding
            }
                |> s_selectIfLoaded (Just (SelectAddress { currency = currency, address = address }))
                |> loadAddress
                    plugins
                    { currency = currency
                    , address = address
                    , table = Nothing
                    , at = Nothing
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
loadAddress plugins { currency, address, table, at } model =
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
                                |> Maybe.withDefault (n browser)

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
                            }
                        |> ApiEffect
                  ]
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
                { label = input.label.input
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
                |> Maybe.withDefault (n browser)

        linkId =
            ( source.id, link.node.id )

        newmodel =
            deselect model
    in
    ( { newmodel
        | browser = browser2
        , selected = SelectedAddresslink linkId
        , layers = Layer.updateAddressLink linkId (\l -> { l | selected = True }) newmodel.layers
      }
    , effects
    )


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
                |> Maybe.withDefault (n browser)

        linkId =
            ( source.id, link.node.id )

        newmodel =
            deselect model
    in
    ( { newmodel
        | browser = browser2
        , selected = SelectedEntitylink ( source.id, link.node.id )
        , layers = Layer.updateEntityLink linkId (\l -> { l | selected = True }) newmodel.layers
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
