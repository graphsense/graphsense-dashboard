module Update.Graph exposing (..)

import Api.Data
import Config.Graph exposing (maxExpandableNeighbors)
import Config.Update as Update
import Effect exposing (n)
import Effect.Graph exposing (Effect(..))
import Init.Graph.Id as Id
import Model.Graph exposing (..)
import Model.Graph.Browser as Browser
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id as Id exposing (EntityId)
import Model.Graph.Layer as Layer
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
            Adding.checkAddress { currency = address.currency, address = address.address } model.adding
                |> Adding.checkEntity { currency = entity.currency, entity = entity.entity }
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
        | adding = Adding.checkAddress { currency = address.currency, address = address.address } model.adding
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
    { model
        | layers = added.layers
        , config =
            model.config
                |> s_colors added.colors
    }
        |> n


update : Update.Config -> Msg -> Model -> ( Model, List Effect )
update uc msg model =
    case msg of
        BrowserGotSvgElement result ->
            result
                |> Result.map
                    (\{ element } ->
                        { model
                            | width = element.width
                            , height = element.height
                        }
                    )
                |> Result.withDefault model
                |> n

        UserWheeledOnGraph x y z ->
            { model
                | transform =
                    Transform.wheel
                        { height = model.height
                        , width = model.width
                        }
                        x
                        y
                        z
                        model.transform
            }
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
                        | layers = Layer.moveEntity id vector model.layers
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
                            , selected = AddressId id |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserRightClickedAddress id ->
            n model

        UserHoversAddress id ->
            n model

        UserLeavesAddress id ->
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
                            , selected = EntityId id |> Just
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserRightClickedEntity id ->
            n model

        UserHoversEntity id ->
            n model

        UserLeavesEntity id ->
            n model

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

        BrowserGotEntityNeighbors id isOutgoing neighbors ->
            let
                -- TODO optimize which only_ids to get for which direction
                onlyIds =
                    model.layers
                        |> Layer.entities
                        |> List.map (.entity >> .entity)

                effect isOut entity =
                    GetEntityNeighborsEffect
                        { currency = Id.currency id
                        , entity = entity
                        , isOutgoing = isOut
                        , onlyIds = Just onlyIds
                        , pagesize = List.length onlyIds
                        , toMsg = BrowserGotEntityEgonet entity isOut
                        }

                ( newModel, new ) =
                    addNeighbors uc id isOutgoing neighbors model

                aligned =
                    neighbors.neighbors
                        |> List.filterMap
                            (\neighbor ->
                                let
                                    entityId =
                                        Id.initEntityId
                                            { currency = Id.currency id
                                            , layer =
                                                Id.layer id
                                                    + (if isOutgoing then
                                                        1

                                                       else
                                                        -1
                                                      )
                                            , id = neighbor.entity.entity
                                            }
                                in
                                if Set.member entityId new then
                                    Layer.getEntity entityId newModel.layers
                                        |> Maybe.map (\entity -> ( entity, neighbor ))

                                else
                                    Nothing
                            )

                _ =
                    Debug.log "entityids" (List.map (first >> (\e -> ( e.id, e.y ))) aligned)
            in
            ( addLinks id isOutgoing aligned newModel
            , neighbors.neighbors
                |> List.map
                    (\entity ->
                        [ effect True entity.entity.entity
                        , effect False entity.entity.entity
                        ]
                    )
                |> List.concat
            )

        BrowserGotEntityEgonet id isOutgoing neighbors ->
            n model

        UserClickedAddressExpandHandle id isOutgoing ->
            n model

        NoOp ->
            n model


addingAddress : { currency : String, address : String } -> Model -> ( Model, List Effect )
addingAddress { currency, address } model =
    { model
        | adding = Adding.addAddress { currency = currency, address = address } model.adding
    }
        |> n


addingEntity : { currency : String, entity : Int } -> Model -> ( Model, List Effect )
addingEntity { currency, entity } model =
    { model
        | adding = Adding.addEntity { currency = currency, entity = entity } model.adding
    }
        |> n


addingLabel : String -> Model -> ( Model, List Effect )
addingLabel label model =
    { model
        | adding = Adding.addLabel label model.adding
    }
        |> n


updateSize : Int -> Int -> Model -> Model
updateSize w h model =
    { model
        | width = model.width + toFloat w
        , height = model.height + toFloat h
    }


addNeighbors : Update.Config -> EntityId -> Bool -> Api.Data.NeighborEntities -> Model -> ( Model, Set EntityId )
addNeighbors uc id isOutgoing { neighbors } model =
    let
        acc =
            Layer.addNeighbors uc id isOutgoing model.config.colors (List.map .entity neighbors) model.layers
    in
    ( { model
        | layers = acc.layers
        , config =
            model.config
                |> s_colors acc.colors
      }
    , Set.union acc.repositioned acc.new
    )


addLinks : EntityId -> Bool -> List ( Entity, Api.Data.NeighborEntity ) -> Model -> Model
addLinks anchor isOutgoing neighbors model =
    let
        layers =
            if isOutgoing then
                Layer.updateLinks { currency = Id.currency anchor, entity = Id.entityId anchor } neighbors model.layers

            else
                model.layers
                    |> Layer.getEntity anchor
                    |> Maybe.map
                        (\entity ->
                            neighbors
                                |> List.foldl
                                    (\( neighbor, neighborEntity ) ->
                                        Layer.updateLinks
                                            { currency = Id.currency neighbor.id, entity = Id.entityId neighbor.id }
                                            [ ( entity, neighborEntity ) ]
                                    )
                                    model.layers
                        )
                    |> Maybe.withDefault model.layers
    in
    { model
        | layers = layers
    }
