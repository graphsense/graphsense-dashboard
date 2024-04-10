module View.Pathfinder exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Lazy exposing (..)
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Network exposing (Network)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Graph
import Util.View exposing (none)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Network as Network
import View.Search



-- Styles


lighterGreyColor : Css.Color
lighterGreyColor =
    Css.rgb 208 216 220


lightGreyColor : Css.Color
lightGreyColor =
    Css.rgb 120 144 156


defaultBackground : Css.Color
defaultBackground =
    Css.rgb 255 255 255


boxStyle : List Css.Style
boxStyle =
    [ Css.backgroundColor defaultBackground
    , Css.boxShadow5 (Css.px 1) (Css.px 1) (Css.px 5) (Css.px 1) lighterGreyColor
    , Css.px 10 |> Css.padding
    ]


searchInputStyle : String -> List Css.Style
searchInputStyle _ =
    [ Css.pct 100 |> Css.width
    , Css.px 20 |> Css.height
    , Css.display Css.block
    , Css.color lightGreyColor
    , Css.border3 (Css.px 2) Css.solid lighterGreyColor
    , Css.backgroundColor defaultBackground
    , Css.px 3 |> Css.borderRadius
    , Css.px 25 |> Css.textIndent
    ]


propertyBoxHeading : List Css.Style
propertyBoxHeading =
    [ Css.fontWeight Css.bold
    , Css.em 1.3 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


toolItemStyle : List Css.Style
toolItemStyle =
    [ Css.px 55 |> Css.minWidth
    , Css.textAlign Css.center
    ]


toolButtonStyle : List Css.Style
toolButtonStyle =
    [ Css.textAlign Css.center, Css.cursor Css.pointer ]


toolIconStyle : List Css.Style
toolIconStyle =
    [ Css.em 1.3 |> Css.fontSize
    , Css.px 5 |> Css.marginBottom
    ]


topLeftPanelStyle : List Css.Style
topLeftPanelStyle =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.left
    , Css.px 10 |> Css.top
    ]


graphToolsStyle : List Css.Style
graphToolsStyle =
    [ Css.position Css.absolute
    , Css.pct 50 |> Css.left
    , Css.px 50 |> Css.bottom
    , Css.displayFlex
    , Css.transform (Css.translate (Css.pct -50))
    ]
        ++ boxStyle


propertiesAndSearchStyle : List Css.Style
propertiesAndSearchStyle =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.right
    , Css.px 10 |> Css.top
    ]


searchBoxStyle : List Css.Style
searchBoxStyle =
    [ Css.px 300 |> Css.minWidth
    , Css.px 10 |> Css.paddingBottom
    ]
        ++ boxStyle


propertyBoxStyle : List Css.Style
propertyBoxStyle =
    searchBoxStyle


graphActionButtonStyle : List Css.Style
graphActionButtonStyle =
    [ Css.px 5 |> Css.margin
    , Css.padding4 (Css.px 3) (Css.px 10) (Css.px 3) (Css.px 10)
    , Css.color lightGreyColor
    , Css.borderColor lighterGreyColor
    , Css.backgroundColor defaultBackground
    , Css.border3 (Css.px 1) Css.solid lighterGreyColor
    , Css.px 3 |> Css.borderRadius
    ]


view : Plugins -> ModelState -> View.Config -> Model -> { navbar : List (Html Msg), contents : List (Html Msg) }
view plugins states vc model =
    { navbar = []
    , contents = graph plugins states vc {} model
    }


graph : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> List (Html Msg)
graph plugins states vc gc model =
    [ vc.size
        |> Maybe.map (graphSvg plugins states vc gc model)
        |> Maybe.withDefault none
    ]
        ++ [ topLeftPanel plugins states vc gc model
           , graphTools plugins states vc gc model
           , topRightPanel plugins states vc gc model
           ]


topLeftPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
topLeftPanel plugins ms vc gc model =
    div [ topLeftPanelStyle |> HA.css ]
        [ h2 [ vc.theme.heading2 |> HA.css ] [ Html.text "Pathfinder" ]

        --, settingsView plugins ms vc gc model
        ]


settingsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
settingsView plugins _ vc gc model =
    div [ (boxStyle ++ [ Css.displayFlex, Css.justifyContent Css.flexEnd ]) |> HA.css ] [ Html.text "Display", FontAwesome.icon FontAwesome.chevronDown |> Html.fromUnstyled ]


graphTools : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
graphTools plugins _ vc gc model =
    div
        [ graphToolsStyle |> HA.css
        ]
        [ graphToolButton FontAwesome.trash (Locale.string vc.locale "restart")
        , graphToolButton FontAwesome.redo (Locale.string vc.locale "redo")
        , graphToolButton FontAwesome.undo (Locale.string vc.locale "undo")
        , graphToolButton FontAwesome.highlighter (Locale.string vc.locale "highlight")
        ]


graphToolButton : FontAwesome.Icon -> String -> Svg Msg
graphToolButton faIcon text =
    div [ toolItemStyle |> HA.css ]
        [ Html.a [ toolButtonStyle |> HA.css ]
            [ div [ toolIconStyle |> HA.css ] [ FontAwesome.icon faIcon |> Html.fromUnstyled ]
            , Html.text text
            ]
        ]


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
topRightPanel plugins ms vc gc model =
    div [ propertiesAndSearchStyle |> HA.css ]
        [ graphActionsView plugins ms vc gc model
        , searchBoxView plugins ms vc gc model
        , propertyBoxView plugins ms vc gc model
        ]


graphActionsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
graphActionsView plugins _ vc gc model =
    div [ [ Css.displayFlex, Css.justifyContent Css.flexEnd, Css.px 5 |> Css.margin ] |> HA.css ]
        [ graphActionButton FontAwesome.arrowUp (Locale.string vc.locale "Import file")
        , graphActionButton FontAwesome.arrowDown (Locale.string vc.locale "Export graph")
        ]


graphActionButton : FontAwesome.Icon -> String -> Svg Msg
graphActionButton faIcon text =
    button [ graphActionButtonStyle |> HA.css ] (iconWithText faIcon text)


iconWithText : FontAwesome.Icon -> String -> List (Html Msg)
iconWithText faIcon text =
    [ span [ [ Css.px 5 |> Css.paddingRight ] |> HA.css ] [ FontAwesome.icon faIcon |> Html.fromUnstyled ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
searchBoxView plugins _ vc gc model =
    div
        [ searchBoxStyle |> HA.css ]
        [ h3 [ propertyBoxHeading |> HA.css ] [ Html.text (Locale.string vc.locale "Add to graph") ]
        , View.Search.search plugins vc { css = searchInputStyle, multiline = False, resultsAsLink = True, showIcon = False } model.search |> Html.map SearchMsg
        ]


propertyBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
propertyBoxView plugins _ vc gc model =
    div
        [ propertyBoxStyle |> HA.css ]
        [ h3 [ propertyBoxHeading |> HA.css ] [ Html.text (Locale.string vc.locale "Add to graph") ]
        ]


graphSvg : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> BBox -> Svg Msg
graphSvg plugins _ vc gc model bbox =
    let
        dim =
            { width = bbox.width, height = bbox.height }
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , Css.Graph.svgRoot vc |> Svg.css
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
            (Util.Graph.decodeCoords Coords
                |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
            )
         ]
            ++ (if model.dragging /= NoDragging then
                    Svg.preventDefaultOn "mousemove"
                        (Util.Graph.decodeCoords Coords
                            |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                        )
                        |> List.singleton

                else
                    []
               )
        )
        [ model.network |> Maybe.map (Svg.lazy4 networks plugins vc gc) |> Maybe.withDefault (div [] [])
        ]


networks : Plugins -> View.Config -> Pathfinder.Config -> Network -> Svg Msg
networks plugins vc gc network =
    Keyed.node "g"
        []
        [ ( "Pathfinder.network." ++ network.name
          , Svg.lazy4 Network.addresses plugins vc gc network.addresses
          )
        ]
