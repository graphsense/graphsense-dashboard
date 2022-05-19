module Update.Graph exposing (..)

import Api.Data
import Config.Graph exposing (maxExpandableNeighbors)
import Config.Update as Update
import Effect exposing (n)
import Effect.Graph exposing (Effect(..))
import Init.Graph.Id as Id
import IntDict exposing (IntDict)
import Log
import Maybe.Extra
import Model.Graph exposing (..)
import Model.Graph.Address exposing (Address)
import Model.Graph.Browser as Browser
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id as Id exposing (EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Msg.Graph as Msg exposing (Msg(..))
import RecordSetter exposing (..)
import Route
import Set exposing (Set)
import Tuple exposing (..)
import Update.Graph.Adding as Adding
import Update.Graph.Color as Color
import Update.Graph.Layer as Layer
import Update.Graph.Transform as Transform


addAddressAndEntity : Update.Config -> Api.Data.Address -> Api.Data.Entity -> Model -> ( Model, List Effect )
addAddressAndEntity uc address entity model =
    let
        addedEntity =
            Layer.addEntity uc model.config.colors entity model.layers

        added =
            Layer.addAddress uc addedEntity.colors address addedEntity.layers

        adding =
            Adding.removeAddress { currency = address.currency, address = address.address } model.adding
    in
    { model
        | adding = adding
        , layers = added.layers
        , config =
            model.config
                |> s_colors added.colors
    }
        |> n


addAddress : Update.Config -> Api.Data.Address -> Model -> ( Model, List Effect )
addAddress uc address model =
    let
        added =
            Layer.addAddress uc model.config.colors address model.layers
    in
    { model
        | adding =
            if Set.isEmpty added.new then
                model.adding

            else
                Adding.removeAddress { currency = address.currency, address = address.address } model.adding
        , layers = added.layers
        , config =
            model.config
                |> s_colors added.colors
    }
        |> n


addEntity : Update.Config -> Api.Data.Entity -> Model -> ( Model, List Effect )
addEntity uc entity model =
    let
        added =
            Layer.addEntity uc model.config.colors entity model.layers
    in
    n
        { model
            | layers = added.layers
            , config =
                model.config
                    |> s_colors added.colors
        }


update : Update.Config -> Msg -> Model -> ( Model, List Effect )
update uc msg model =
    case msg of
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

        UserClickedGraph ->
            { model
                | selected = SelectedNone
                , browser =
                    model.browser
                        |> s_visible False
            }
                |> n

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
                            Dragging model.transform coords

                        x ->
                            x
            }
                |> n

        UserPushesLeftMouseButtonOnEntity id coords ->
            { model
                | dragging =
                    case model.dragging of
                        NoDragging ->
                            DraggingNode id coords

                        x ->
                            x
            }
                |> n

        UserMovesMouseOnGraph coords ->
            (case model.dragging of
                NoDragging ->
                    model

                Dragging transform start ->
                    { model
                        | transform = Transform.update start coords transform
                    }

                DraggingNode id start ->
                    let
                        vector =
                            Transform.vector start coords model.transform
                    in
                    { model
                        | layers =
                            Layer.moveEntity id vector model.layers
                                |> Layer.syncLinks (Set.singleton id)
                    }
            )
                |> n

        UserReleasesMouseButton ->
            (case model.dragging of
                NoDragging ->
                    model

                Dragging _ _ ->
                    { model
                        | dragging = NoDragging
                    }

                DraggingNode id _ ->
                    { model
                        | layers = Layer.releaseEntity id model.layers
                        , dragging = NoDragging
                    }
            )
                |> n

        UserClickedAddress id ->
            Layer.getAddress id model.layers
                |> Maybe.map
                    (\address ->
                        { model
                            | browser =
                                model.browser
                                    |> s_visible True
                                    |> s_type_ (Browser.Address address)
                            , selected = SelectedAddress id
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserRightClickedAddress id ->
            n model

        UserHoversAddress id ->
            n model

        UserClickedEntity id ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\entity ->
                        { model
                            | browser =
                                model.browser
                                    |> s_visible True
                                    |> s_type_ (Browser.Entity entity)
                            , selected = SelectedEntity id
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserRightClickedEntity id ->
            n model

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
                          , toMsg = BrowserGotEntityNeighbors id isOutgoing
                          }
                            |> GetEntityNeighborsEffect
                            |> List.singleton
                        )

                    else
                        n model

        BrowserGotAddress address ->
            addAddress uc
                address
                { model
                    | adding = Adding.setAddress { currency = address.currency, address = address.address } address model.adding
                }

        BrowserGotEntity a entity ->
            model.adding
                |> Adding.getAddress { currency = entity.currency, address = a }
                |> Maybe.map
                    (\address ->
                        addAddressAndEntity uc address entity model
                    )
                |> Maybe.Extra.withDefaultLazy (\_ -> addEntity uc entity model)
                |> mapSecond
                    ((++)
                        (getEntityEgonet
                            { currency = entity.currency
                            , entity = entity.entity
                            }
                            model.layers
                        )
                    )

        BrowserGotEntityNeighbors id isOutgoing neighbors ->
            Layer.getEntity id model.layers
                |> Maybe.map
                    (\anchor ->
                        handleEntityNeighbors uc anchor isOutgoing neighbors.neighbors model
                    )
                |> Maybe.withDefault (n model)

        BrowserGotEntityEgonet currency id isOutgoing neighbors ->
            addEntityEgonet currency id isOutgoing neighbors.neighbors model
                |> n

        BrowserGotAddressNeighbors id isOutgoing neighbors ->
            ( model
            , neighbors.neighbors
                |> List.map
                    (\neighbor ->
                        GetEntityForAddressEffect
                            { address = neighbor.address.address
                            , currency = neighbor.address.currency
                            , toMsg =
                                BrowserGotEntityForAddressNeighbor
                                    { anchor = id
                                    , isOutgoing = isOutgoing
                                    , neighbor = neighbor
                                    }
                            }
                    )
            )

        BrowserGotEntityForAddressNeighbor { anchor, isOutgoing, neighbor } entity ->
            Layer.getAddress anchor model.layers
                |> Maybe.andThen
                    (\address ->
                        Layer.getEntity address.entityId model.layers
                            |> Maybe.map (pair address)
                    )
                |> Maybe.map
                    (\( address, ent ) ->
                        handleAddressNeighbor uc ( address, ent ) isOutgoing ( neighbor, entity ) model
                    )
                |> Maybe.withDefault (n model)
                |> mapSecond
                    ((++)
                        (getEntityEgonet
                            { currency = entity.currency
                            , entity = entity.entity
                            }
                            model.layers
                        )
                    )

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
                          , toMsg = BrowserGotAddressNeighbors id isOutgoing
                          }
                            |> GetAddressNeighborsEffect
                            |> List.singleton
                        )

                    else
                        n model

        BrowserGotNow time ->
            { model
                | browser =
                    model.browser
                        |> s_now time
            }
                |> n

        NoOp ->
            n model


updateByUrl : String -> Route.Thing -> Model -> ( Model, List Effect )
updateByUrl currency thing model =
    case thing of
        Route.Address a ->
            ( { model
                | adding = Adding.loadAddress { currency = currency, address = a } model.adding |> Log.log "adding"
              }
            , [ GetEntityForAddressEffect
                    { address = a
                    , currency = currency
                    , toMsg = BrowserGotEntity a
                    }
              , GetAddressEffect
                    { address = a
                    , currency = currency
                    , toMsg = BrowserGotAddress
                    }
              ]
            )

        Route.Tx t ->
            n model

        Route.Block b ->
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


addAddressNeighborWithEntity : Update.Config -> ( Address, Entity ) -> Bool -> ( Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> ( Model, Maybe Address, Set EntityId )
addAddressNeighborWithEntity uc ( anchorAddress, anchorEntity ) isOutgoing ( neighbor, entity ) model =
    let
        acc =
            Layer.addEntityNeighbors uc anchorEntity isOutgoing model.config.colors [ entity ] model.layers
    in
    Set.toList acc.new
        |> List.head
        |> Maybe.map
            (\new ->
                let
                    added =
                        Layer.addAddressAtEntity uc model.config.colors new neighbor.address acc.layers
                in
                ( { model
                    | layers = added.layers
                  }
                , Set.toList added.new
                    |> List.head
                    |> Maybe.andThen
                        (\a -> Layer.getAddress a added.layers)
                , added.repositioned
                )
            )
        |> Maybe.withDefault
            ( model
            , Nothing
            , Set.empty
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
        layers =
            if isOutgoing then
                Layer.updateEntityLinks { currency = Id.currency anchor.id, entity = Id.entityId anchor.id } neighbors model.layers

            else
                neighbors
                    |> List.foldl
                        (\( neighborEntity, neighbor ) ->
                            Layer.updateEntityLinks
                                { currency = Id.currency neighbor.id, entity = Id.entityId neighbor.id }
                                [ ( neighborEntity, anchor ) ]
                        )
                        model.layers
    in
    { model
        | layers = layers
    }


addAddressLink : Address -> Bool -> ( Api.Data.NeighborAddress, Address ) -> Model -> Model
addAddressLink anchor isOutgoing ( neighbor, target ) model =
    let
        layers =
            if isOutgoing then
                Layer.updateAddressLink { currency = Id.currency anchor.id, address = Id.addressId anchor.id } ( neighbor, target ) model.layers

            else
                Layer.updateAddressLink
                    { currency = Id.currency target.id, address = Id.addressId target.id }
                    ( neighbor, anchor )
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


handleEntityNeighbors : Update.Config -> Entity -> Bool -> List Api.Data.NeighborEntity -> Model -> ( Model, List Effect )
handleEntityNeighbors uc anchor isOutgoing neighbors model =
    let
        ( newModel, new, repositioned ) =
            addEntityNeighbors uc anchor isOutgoing neighbors model
    in
    ( addEntityLinks anchor isOutgoing new newModel
        |> syncLinks repositioned
    , neighbors
        |> List.map (\{ entity } -> getEntityEgonet { currency = entity.currency, entity = entity.entity } newModel.layers)
        |> List.concat
    )


getEntityEgonet : { currency : String, entity : Int } -> IntDict Layer -> List Effect
getEntityEgonet { currency, entity } layers =
    let
        -- TODO optimize which only_ids to get for which direction
        onlyIds =
            layers
                |> Layer.entities
                |> List.map (.entity >> .entity)

        effect isOut =
            GetEntityNeighborsEffect
                { currency = currency
                , entity = entity
                , isOutgoing = isOut
                , onlyIds = Just onlyIds
                , pagesize = List.length onlyIds
                , toMsg = BrowserGotEntityEgonet currency entity isOut
                }
    in
    [ effect True
    , effect False
    ]


handleAddressNeighbor : Update.Config -> ( Address, Entity ) -> Bool -> ( Api.Data.NeighborAddress, Api.Data.Entity ) -> Model -> ( Model, List Effect )
handleAddressNeighbor uc anchor isOutgoing neighbor model =
    let
        ( newModel, new, repositionedEntities ) =
            addAddressNeighborWithEntity uc anchor isOutgoing neighbor model
    in
    case new of
        Nothing ->
            n model

        Just address ->
            ( addAddressLink (first anchor) isOutgoing ( first neighbor, address ) newModel
                |> Debug.log "addAddressLink"
                |> syncLinks repositionedEntities
            , []
            )


syncLinks : Set EntityId -> Model -> Model
syncLinks repositioned model =
    { model
        | layers = Layer.syncLinks repositioned model.layers
    }
