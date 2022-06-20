module Update.Graph exposing (..)

import Api.Data
import Browser.Dom as Dom
import Config.Graph exposing (maxExpandableAddresses, maxExpandableNeighbors)
import Config.Update as Update
import DateFormat
import Decode.Graph.Graph050 as Graph050
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Graph exposing (Effect(..), getEntityEgonet)
import File
import File.Select
import Init.Graph.ContextMenu as ContextMenu
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
import Model.Graph.Id as Id exposing (EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Link as Link
import Model.Graph.Search as Search
import Model.Graph.Tag as Tag
import Model.Graph.Tool as Tool
import Msg.Graph as Msg exposing (Msg(..))
import Plugin as Plugin exposing (Plugins)
import Plugin.Model as Plugin
import Ports
import Process
import RecordSetter exposing (..)
import Route as R exposing (toUrl)
import Route.Graph as Route
import Set exposing (Set)
import Task
import Time
import Tuple exposing (..)
import Update.Graph.Adding as Adding
import Update.Graph.Address as Address
import Update.Graph.Browser as Browser
import Update.Graph.Color as Color
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
        }
    -> Model
    -> ( Model, List Effect )
addAddress plugins uc { address, entity, incoming, outgoing } model =
    let
        ( newModel, eff ) =
            addEntity plugins
                uc
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
                        |> Layer.syncLinks added.repositioned
                        |> addUserTag added.new model.userAddressTags
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
        |> mapSecond ((++) eff)
        |> mapSecond ((::) getTagsEffect)
        |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))


addEntity : Plugins -> Update.Config -> { entity : Api.Data.Entity, incoming : List Api.Data.NeighborEntity, outgoing : List Api.Data.NeighborEntity } -> Model -> ( Model, List Effect )
addEntity plugins uc { entity, incoming, outgoing } model =
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


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory msg
        |> updateByMsg plugins uc msg


updateByMsg : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
updateByMsg plugins uc msg model =
    case Log.truncate "msg" msg of
        InfiniteScrollMsg m ->
            let
                ( browser, eff ) =
                    Browser.infiniteScroll m model.browser
            in
            ( { model
                | browser = browser
              }
            , eff
            )

        InternalGraphAddedAddresses _ ->
            n model

        InternalGraphAddedEntities _ ->
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
                                , [ GetEntityAddressesEffect
                                        { currency = Id.currency id
                                        , entity = Id.entityId id
                                        , pagesize = maxExpandableAddresses
                                        , nextpage = Nothing
                                        , toMsg = BrowserGotEntityAddresses id
                                        }
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
                        |> addEntity plugins uc added

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
                        |> addEntity plugins uc added

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
                        |> addUserTag added.new model.userAddressTags
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
                |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))

        BrowserGotEntityAddressesForTable id addresses ->
            { model
                | browser = Browser.showEntityAddresses id addresses model.browser
            }
                |> n

        BrowserGotAddressTags id tags ->
            model
                |> updateAddresses id (Address.updateTags tags.addressTags)
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

        BrowserGotTx data ->
            { model
                | browser = Browser.showTx data model.browser
            }
                |> n

        BrowserGotTxUtxoAddresses id isOutgoing data ->
            { model
                | browser = Browser.showTxUtxoAddresses id isOutgoing data model.browser
            }
                |> n

        BrowserGotBlock data ->
            { model
                | browser = Browser.showBlock data model.browser
            }
                |> n

        BrowserGotBlockTxs id data ->
            { model
                | browser =
                    if String.toLower id.currency == "eth" then
                        Browser.showBlockTxsAccount id data model.browser

                    else
                        Browser.showBlockTxsUtxo id data model.browser
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
                                    |> Tag.init id el
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

        UserSubmitsTagInput ->
            model.tag
                |> Maybe.map
                    (\tag ->
                        { label = tag.input.label.input
                        , category =
                            if String.isEmpty tag.input.category then
                                Nothing

                            else
                                Just tag.input.category
                        , abuse =
                            if String.isEmpty tag.input.abuse then
                                Nothing

                            else
                                Just tag.input.abuse
                        , source = tag.input.source
                        , currency =
                            Id.currency tag.input.id
                        , address =
                            Id.addressId tag.input.id
                        }
                    )
                |> Maybe.map (storeUserTag uc model)
                |> Maybe.withDefault model
                |> n

        UserClickedUserTags ->
            { model
                | browser = Debug.todo "Browser.showUserTags model.userAddressTags model.browser"
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
            }
                |> n

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
                | layers =
                    added.layers
                        |> addUserTag added.new model.userAddressTags
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
                |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))

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
                    | layers =
                        added.layers
                            |> addUserTag added.new model.userAddressTags
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
                    |> mapSecond ((::) (InternalGraphAddedAddressesEffect added.new))

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

        ToBeDone id ->
            ( model
            , id
                |> Dom.getElement
                |> Task.attempt BrowserGotElementForTBD
                |> CmdEffect
                |> List.singleton
            )

        BrowserGotElementForTBD element ->
            element
                |> Result.map
                    (\el ->
                        ( { model
                            | hovercardTBD = Just el
                          }
                        , [ Process.sleep 2000
                                |> Task.perform (\_ -> RuntimeHideTBD)
                                |> CmdEffect
                          ]
                        )
                    )
                |> Result.withDefault (n model)

        RuntimeHideTBD ->
            n { model | hovercardTBD = Nothing }

        UserClicksLegend id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Legend _, Just ( el, vis ) ) ->
                    toolVisible model el vis

                _ ->
                    getToolElement model id BrowserGotLegendElement

        BrowserGotLegendElement result ->
            makeLegend model
                |> toolElementResultToTool result model

        UserClicksConfiguraton id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Configuration _, Just ( el, vis ) ) ->
                    toolVisible model el vis

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

                _ ->
                    getToolElement model id BrowserGotExportElement

        BrowserGotExportElement result ->
            toolElementResultToTool result model Tool.Export

        UserClickedImport id ->
            case ( model.activeTool.toolbox, model.activeTool.element ) of
                ( Tool.Import, Just ( el, vis ) ) ->
                    toolVisible model el vis

                _ ->
                    getToolElement model id BrowserGotImportElement

        BrowserGotImportElement result ->
            toolElementResultToTool result model Tool.Import

        UserChangesCurrency currency ->
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
                            | search = Search.init model.entityConcepts element id |> Just
                        }
                    )
                |> Result.withDefault model
                |> n

        UserSelectsDirection direction ->
            updateSearch (Search.selectDirection direction) model

        UserSelectsCriterion criterion ->
            updateSearch
                (Search.selectCriterion
                    { categories = model.entityConcepts
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

        PortDeserializedGS data ->
            -- handled upstream
            n model

        UserClickedUndo ->
            case model.history of
                History (recent :: rest) future ->
                    { model
                        | layers = recent
                        , history =
                            model.layers
                                :: future
                                |> History rest
                    }
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
                                Id.layer anchor.id
                                    + (if isOutgoing then
                                        1

                                       else
                                        -1
                                      )
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


toolVisible : Model -> Dom.Element -> Bool -> ( Model, List Effect )
toolVisible model element visible =
    n
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

        Route.Currency currency (Route.Tx t table) ->
            let
                ( browser, effect ) =
                    if String.toLower currency == "eth" then
                        Browser.loadingTxAccount { currency = currency, txHash = t } model.browser

                    else
                        Browser.loadingTxUtxo { currency = currency, txHash = t } model.browser

                ( browser2, effects ) =
                    if String.toLower currency == "eth" then
                        n browser

                    else
                        table
                            |> Maybe.map (\tb -> Browser.showTxUtxoTable tb browser)
                            |> Maybe.withDefault (n browser)
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
            , [ GetBlockEffect
                    { height = b
                    , currency = currency
                    , toMsg = BrowserGotBlock
                    }
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
                            |> Maybe.map
                                (\link ->
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
                                    in
                                    ( { model
                                        | browser = browser2
                                        , selected = SelectedAddresslink ( source.id, link.node.id )
                                      }
                                    , effects
                                    )
                                )
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
                                (\link ->
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
                                    in
                                    ( { model
                                        | browser = browser2
                                        , selected = SelectedEntitylink ( source.id, link.node.id )
                                      }
                                    , effects
                                    )
                                )
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
                                            Layer.addAddressAtEntity plugins uc model.config.colors new neighbor.address added_.layers
                                    in
                                    { layers =
                                        added__.layers
                                            |> addUserTag added__.new model.userAddressTags
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
                            addAddressLink (first anchor) isOutgoing ( neighbor, address ) model_
                        )
                    |> Maybe.withDefault model_
            )
            added.model
        |> syncLinks added.repositioned
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


storeUserTag : Update.Config -> Model -> Tag.UserTag -> Model
storeUserTag uc model tag =
    if String.isEmpty tag.label then
        model

    else
        let
            colors =
                tag.category
                    |> Color.update uc model.config.colors
        in
        { model
            | userAddressTags =
                Dict.insert ( tag.currency, tag.address ) tag model.userAddressTags
            , config =
                model.config
                    |> s_colors colors
            , tag = Nothing
        }
            |> updateLegend
            |> updateAddresses { currency = tag.currency, address = tag.address } (\a -> { a | userTag = Just tag })


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


updateByPluginOutMsg : Plugins -> String -> Plugin.OutMsgs -> Model -> ( Model, List Effect )
updateByPluginOutMsg plugins pid outMsgs model =
    outMsgs
        |> List.foldl
            (\msg ( mo, eff ) ->
                case Log.log "outMsg" msg of
                    Plugin.ShowBrowser ->
                        ( { mo
                            | browser = Browser.showPlugin pid mo.browser
                          }
                        , eff
                        )

                    Plugin.UpdateAddresses id msgValue ->
                        ( { mo
                            | layers = Layer.updateAddresses id (Plugin.updateAddress pid plugins msgValue) mo.layers
                          }
                            |> refreshBrowserAddress id
                        , eff
                        )

                    Plugin.UpdateAddressEntities id msgValue ->
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
                                                (\i -> Layer.updateEntity i (Plugin.updateEntity pid plugins msgValue))
                                                mo.layers
                                }
                        , eff
                        )

                    Plugin.UpdateEntities id msgValue ->
                        ( { mo
                            | layers = Layer.updateEntities id (Plugin.updateEntity pid plugins msgValue) mo.layers
                          }
                            |> refreshBrowserEntity id
                        , eff
                        )

                    Plugin.GetEntitiesForAddresses _ _ ->
                        ( mo, [] )

                    Plugin.GetEntities _ _ ->
                        ( mo, [] )

                    Plugin.PushGraphUrl url ->
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
    { model
        | browser =
            case model.browser.type_ of
                Browser.Entity (Browser.Loaded en) table ->
                    if en.entity.currency == id.currency && en.entity.entity == id.entity then
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


addUserTag : Set Id.AddressId -> Dict ( String, String ) Tag.UserTag -> IntDict Layer -> IntDict Layer
addUserTag ids userTags layers =
    ids
        |> Set.toList
        |> List.foldl
            (\id layers_ ->
                Dict.get ( Id.currency id, Id.addressId id ) userTags
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
                List.Extra.find (.id >> (==) cat) model.entityConcepts
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
                    (\{ currency, address, label, source, category, abuse } ->
                        Yaml.Encode.record
                            [ ( "currency", Yaml.Encode.string currency )
                            , ( "address", Yaml.Encode.string address )
                            , ( "label", Yaml.Encode.string label )
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
            (\tag mo ->
                storeUserTag uc model tag
            )
            model


decodeYamlTag : Yaml.Decode.Decoder Tag.UserTag
decodeYamlTag =
    Yaml.Decode.map6 Tag.UserTag
        (Yaml.Decode.field "currency" Yaml.Decode.string)
        (Yaml.Decode.field "address" Yaml.Decode.string)
        (Yaml.Decode.field "label" Yaml.Decode.string)
        (Yaml.Decode.oneOf [ Yaml.Decode.field "source" Yaml.Decode.string, Yaml.Decode.succeed "" ])
        (Yaml.Decode.field "category" (Yaml.Decode.maybe Yaml.Decode.string))
        (Yaml.Decode.field "abuse" (Yaml.Decode.maybe Yaml.Decode.string))


deserialize : Json.Decode.Value -> Result Json.Decode.Error Deserialized
deserialize =
    Json.Decode.decodeValue
        (Json.Decode.index 0 Json.Decode.string
            |> Json.Decode.andThen deserializeByVersion
        )


deserializeByVersion : String -> Json.Decode.Decoder Deserialized
deserializeByVersion version =
    case version of
        "0.5.0" ->
            Graph050.decoder

        _ ->
            Json.Decode.fail ("unknown version " ++ version)


pushHistory : Msg -> Model -> Model
pushHistory msg model =
    if shallPushHistory msg model then
        case model.history of
            History past future ->
                { model
                    | history =
                        History
                            (model.layers
                                :: (if List.length past >= maxHistory then
                                        List.take (maxHistory - 1) past

                                    else
                                        past
                                   )
                            )
                            []
                }

    else
        model


shallPushHistory : Msg -> Model -> Bool
shallPushHistory msg model =
    case msg of
        UserClickedEntityExpandHandle _ _ ->
            True

        UserClickedAddressExpandHandle _ _ ->
            True

        UserClickedAddressesExpand _ ->
            True

        UserClickedRemoveAddress _ ->
            True

        UserClickedRemoveEntity _ ->
            True

        UserClickedAddressInEntityAddressesTable _ _ ->
            True

        UserClickedAddressInNeighborsTable _ _ _ ->
            True

        UserClickedEntityInNeighborsTable _ _ _ ->
            True

        UserSubmitsTagInput ->
            True

        UserSubmitsSearchInput ->
            True

        _ ->
            False
