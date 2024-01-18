module View.Graph exposing (view)

import Config.Graph as Graph
import Config.View exposing (Config)
import Css.Graph as Css
import Dict
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Lazy exposing (..)
import IntDict exposing (IntDict)
import Json.Decode
import Maybe.Extra
import Model.Graph exposing (..)
import Model.Graph.Address as Address
import Model.Graph.ContextMenu as ContextMenu
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer exposing (Layer)
import Msg.Graph exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Route
import Route.Graph
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple exposing (..)
import Util.Css
import Util.ExternalLinks exposing (getBlockExplorerLinks, getBlockExplorerTransactionLinks)
import Util.Graph as Util
import Util.View exposing (contextMenuRule, hovercard, none)
import View.Graph.Browser exposing (browser)
import View.Graph.ContextMenu as ContextMenu
import View.Graph.Entity as Entity
import View.Graph.Layer as ViewLayer
import View.Graph.Link as Link
import View.Graph.Navbar as Navbar
import View.Graph.Search as Search
import View.Graph.Tag as Tag
import View.Graph.Tool as Tool
import View.Graph.Transform as Transform
import View.Locale as Locale


view : Plugins -> ModelState -> Config -> Model -> { navbar : List (Html Msg), contents : List (Html Msg) }
view plugins states vc model =
    { navbar = [ Navbar.navbar plugins states vc model ]
    , contents = graph plugins states vc model.config model
    }


graph : Plugins -> ModelState -> Config -> Graph.Config -> Model -> List (Html Msg)
graph plugins states vc gc model =
    [ lazy5 browser plugins states vc gc model.browser
    , Tool.toolbox vc model
    , vc.size
        |> Maybe.map (graphSvg plugins states vc gc model)
        |> Maybe.withDefault none
    , model.contextMenu |> Maybe.map (contextMenu plugins states vc model) |> Maybe.withDefault none
    ]
        ++ hovercards plugins states vc model


graphSvg : Plugins -> ModelState -> Config -> Graph.Config -> Model -> BBox -> Svg Msg
graphSvg plugins _ vc gc model bbox =
    let
        dim =
            { width = bbox.width, height = bbox.height }
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , Css.svgRoot vc |> Svg.css
         , UserClickedGraph model.dragging
            |> onClick
         , HA.id "graph"
         , Svg.custom "wheel"
            (Json.Decode.map3
                (\y mx my ->
                    { message = UserWheeledOnGraph mx my y
                    , stopPropagation = False
                    , preventDefault = False
                    }
                )
                (Json.Decode.field "deltaY" Json.Decode.float)
                (Json.Decode.field "offsetX" Json.Decode.float)
                (Json.Decode.field "offsetY" Json.Decode.float)
            )
         , Svg.on "mousedown"
            (Util.decodeCoords Coords
                |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
            )
         ]
            ++ (if model.dragging /= NoDragging then
                    Svg.preventDefaultOn "mousemove"
                        (Util.decodeCoords Coords
                            |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                        )
                        |> List.singleton

                else
                    []
               )
        )
        ([ Svg.lazy2 arrowMarkers vc gc
         ]
            ++ (if gc.showEntityShadowLinks then
                    [ Svg.lazy2 entityShadowLinks vc model.layers ]

                else
                    []
               )
            ++ [ Svg.lazy3 entityLinks vc gc model.layers
               , Svg.lazy4 entities plugins vc gc model.layers
               ]
            ++ (if gc.showAddressShadowLinks then
                    [ Svg.lazy2 addressShadowLinks vc model.layers ]

                else
                    []
               )
            ++ [ Svg.lazy3 addressLinks vc gc model.layers

               --, Svg.lazy4 showOrigin plugins vc gc model.layers
               --, Svg.lazy4 showLayerBoundingBox plugins vc gc model.layers
               , Svg.lazy4 addresses plugins vc gc model.layers
               , Svg.lazy4 hoveredLinks vc gc model.hovered model.layers
               ]
        )


addresses : Plugins -> Config -> Graph.Config -> IntDict Layer -> Svg Msg
addresses plugins vc gc layers =
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "Graph.layer" ++ String.fromInt layerId
                , Svg.lazy4 ViewLayer.addresses plugins vc gc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entities : Plugins -> Config -> Graph.Config -> IntDict Layer -> Svg Msg
entities plugins vc gc layers =
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "Graph.layer" ++ String.fromInt layerId
                , Svg.lazy4 ViewLayer.entities plugins vc gc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entityShadowLinks : Config -> IntDict Layer -> Svg Msg
entityShadowLinks vc layers =
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "Graph.entityShadowLinks" ++ String.fromInt layerId
                , Svg.lazy2 ViewLayer.entityShadowLinks vc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


addressShadowLinks : Config -> IntDict Layer -> Svg Msg
addressShadowLinks vc layers =
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "Graph.addressShadowLinks" ++ String.fromInt layerId
                , Svg.lazy2 ViewLayer.addressShadowLinks vc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entityLinks : Config -> Graph.Config -> IntDict Layer -> Svg Msg
entityLinks vc gc layers =
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "Graph.entityLinks" ++ String.fromInt layerId
                , Svg.lazy3 ViewLayer.entityLinks vc gc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


addressLinks : Config -> Graph.Config -> IntDict Layer -> Svg Msg
addressLinks vc gc layers =
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "Graph.addressLinks" ++ String.fromInt layerId
                , Svg.lazy3 ViewLayer.addressLinks vc gc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


hoveredLinks : Config -> Graph.Config -> Hovered -> IntDict Layer -> Svg Msg
hoveredLinks vc gc hovered layers =
    g []
        (case hovered of
            HoveredEntityLink id ->
                Id.getSourceId id
                    |> Id.layer
                    |> (\l -> IntDict.get l layers)
                    |> Maybe.map (ViewLayer.calcRange vc gc)
                    |> Maybe.andThen
                        (\( mn, mx ) ->
                            Layer.getEntityLink id layers
                                |> Maybe.map
                                    (\( source, target ) ->
                                        [ Svg.lazy6 Link.entityLinkHovered vc gc mn mx source target
                                        ]
                                    )
                        )
                    |> Maybe.withDefault []

            HoveredAddressLink id ->
                Id.getSourceId id
                    |> Id.layer
                    |> (\l -> IntDict.get l layers)
                    |> Maybe.map (ViewLayer.calcAddressRange vc gc)
                    |> Maybe.andThen
                        (\( mn, mx ) ->
                            Layer.getAddressLink id layers
                                |> Maybe.map
                                    (\( source, target ) ->
                                        [ Svg.lazy6 Link.addressLinkHovered vc gc mn mx source target
                                        ]
                                    )
                        )
                    |> Maybe.withDefault []

            HoveredAddress id ->
                Id.layer id
                    - 1
                    |> (\l -> IntDict.get l layers)
                    |> Maybe.map
                        (\layer ->
                            ViewLayer.calcAddressRange vc gc layer
                                |> (\( mn, mx ) ->
                                        Layer.getAddressLinksByTarget id layer
                                            |> List.map
                                                (\( source, target ) ->
                                                    Svg.lazy6 Link.addressLinkHovered vc gc mn mx source target
                                                )
                                   )
                        )
                    |> Maybe.withDefault []
                    |> (++)
                        (Maybe.map2
                            (\layer source ->
                                ViewLayer.calcAddressRange vc gc layer
                                    |> (\( mn, mx ) ->
                                            case source.links of
                                                Address.Links links ->
                                                    links
                                                        |> Dict.values
                                                        |> List.map
                                                            (Svg.lazy6 Link.addressLinkHovered vc gc mn mx source)
                                       )
                            )
                            (IntDict.get (Id.layer id) layers)
                            (Layer.getAddress id layers)
                            |> Maybe.withDefault []
                        )

            HoveredEntity id ->
                Id.layer id
                    - 1
                    |> (\l -> IntDict.get l layers)
                    |> Maybe.map
                        (\layer ->
                            ViewLayer.calcRange vc gc layer
                                |> (\( mn, mx ) ->
                                        Layer.getEntityLinksByTarget id layer
                                            |> List.filterMap
                                                (\( source, target ) ->
                                                    if Entity.showLink source target then
                                                        Svg.lazy6 Link.entityLinkHovered vc gc mn mx source target
                                                            |> Just

                                                    else
                                                        Nothing
                                                )
                                   )
                        )
                    |> Maybe.withDefault []
                    |> (++)
                        (Maybe.map2
                            (\layer source ->
                                ViewLayer.calcRange vc gc layer
                                    |> (\( mn, mx ) ->
                                            case source.links of
                                                Entity.Links links ->
                                                    links
                                                        |> Dict.values
                                                        |> List.filterMap
                                                            (\target ->
                                                                if Entity.showLink source target then
                                                                    Svg.lazy6 Link.entityLinkHovered vc gc mn mx source target
                                                                        |> Just

                                                                else
                                                                    Nothing
                                                            )
                                       )
                            )
                            (IntDict.get (Id.layer id) layers)
                            (Layer.getEntity id layers)
                            |> Maybe.withDefault []
                        )

            HoveredNone ->
                []
        )


arrowMarkers : Config -> Graph.Config -> Svg Msg
arrowMarkers vc gc =
    [ vc.theme.graph.linkColorFaded vc.lightmode
    , vc.theme.graph.linkColorStrong vc.lightmode
    , vc.theme.graph.linkColorSelected vc.lightmode
    ]
        ++ vc.theme.graph.highlightsColorScheme
        |> List.map (Link.arrowMarker vc gc)
        |> defs []


contextMenu : Plugins -> ModelState -> Config -> Model -> ContextMenu.Model -> Html Msg
contextMenu plugins states vc model cm =
    let
        option title msg =
            ContextMenu.option vc (Locale.string vc.locale title) msg

        optionWithIcon title icon msg =
            ContextMenu.optionWithIcon vc (Locale.string vc.locale title) icon msg

        addBlockExplorerLinks currency address =
            getBlockExplorerLinks currency address
                |> List.map
                    (\( url, label ) ->
                        optionWithIcon label FontAwesome.externalLinkAlt (UserClickedExternalLink url)
                    )

        addBlockExplorerTransactionLinks currency txHash =
            getBlockExplorerTransactionLinks currency txHash
                |> List.map
                    (\( url, label ) ->
                        optionWithIcon label FontAwesome.externalLinkAlt (UserClickedExternalLink url)
                    )
    in
    (case cm.type_ of
        ContextMenu.Address address ->
            [ UserClickedAnnotateAddress address.id
                |> optionWithIcon "Annotate address" FontAwesome.userTag
            , UserClickedRemoveAddress address.id
                |> optionWithIcon "Remove from graph" FontAwesome.eraser
            , Route.Graph.addressRoute
                { currency = address.address.currency
                , address = address.address.address
                , layer = Nothing
                , table = Nothing
                }
                |> Route.graphRoute
                |> Route.toUrl
                |> UserClickedExternalLink
                |> optionWithIcon "Open in new tab" FontAwesome.externalLinkAlt
            ]
                ++ contextMenuRule vc
                ++ addBlockExplorerLinks address.address.currency address.address.address
                ++ Plugin.addressContextMenu plugins states vc address

        ContextMenu.Entity entity ->
            [ UserClickedAnnotateEntity entity.id
                |> optionWithIcon "Annotate entity" FontAwesome.userTag
            , UserClickedSearch entity.id
                |> optionWithIcon "Search neighbors" FontAwesome.search
            , UserClickedRemoveEntity entity.id
                |> optionWithIcon "Remove from graph" FontAwesome.eraser
            ]

        ContextMenu.Transaction txHash currency ->
            addBlockExplorerTransactionLinks currency txHash

        ContextMenu.AddressLink id ->
            let
                srcLink =
                    Maybe.Extra.andThen2
                        (\source target ->
                            case source.links of
                                Entity.Links lnks ->
                                    Dict.get target.id lnks
                                        |> Maybe.map (pair source)
                        )
                        (Layer.getAddress (Id.getSourceId id) model.layers
                            |> Maybe.andThen (\a -> Layer.getEntity a.entityId model.layers)
                        )
                        (Layer.getAddress (Id.getTargetId id) model.layers
                            |> Maybe.andThen (\a -> Layer.getEntity a.entityId model.layers)
                        )
            in
            (UserClickedRemoveAddressLink id
                |> optionWithIcon "Remove link" FontAwesome.eraser
            )
                :: (srcLink
                        |> Maybe.map
                            (\( src, li ) ->
                                let
                                    lbl =
                                        if li.forceShow then
                                            "Hide entity link"

                                        else
                                            "Show entity link"
                                in
                                not li.forceShow
                                    |> UserClickedForceShowEntityLink
                                        ( src.id, li.node.id )
                                    |> option lbl
                                    |> List.singleton
                            )
                        |> Maybe.withDefault []
                   )

        ContextMenu.EntityLink id ->
            [ UserClickedRemoveEntityLink id
                |> optionWithIcon "Remove" FontAwesome.eraser
            ]
    )
        |> ContextMenu.view vc cm.coords


hovercards : Plugins -> ModelState -> Config -> Model -> List (Html Msg)
hovercards plugins states vc model =
    let
        zIndex =
            Util.Css.zIndexMainValue - 1
    in
    (model.tag
        |> Maybe.map
            (\tag ->
                (Tag.inputHovercard plugins
                    vc
                    { entityConcepts = model.config.entityConcepts
                    , abuseConcepts = model.config.abuseConcepts
                    }
                    tag
                    |> Html.Styled.toUnstyled
                    |> List.singleton
                )
                    |> hovercard vc tag.hovercard zIndex
            )
        |> Maybe.withDefault []
    )
        ++ (model.search
                |> Maybe.map
                    (\search ->
                        (Search.inputHovercard plugins vc search
                            |> Html.Styled.toUnstyled
                            |> List.singleton
                        )
                            |> hovercard vc search.hovercard zIndex
                    )
                |> Maybe.withDefault []
           )
        ++ Plugin.hovercards plugins states vc
