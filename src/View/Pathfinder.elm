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
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Network as Network
import View.Search



-- Styles
-- http://probablyprogramming.com/2009/03/15/the-tiniest-gif-ever


dummyImageSrc : View.Config -> String
dummyImageSrc _ =
    "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="


lighterGreyColor : Css.Color
lighterGreyColor =
    Css.rgb 208 216 220


lightGreyColor : Css.Color
lightGreyColor =
    Css.rgb 120 144 156


defaultBackgroundColor : Css.Color
defaultBackgroundColor =
    Css.rgb 255 255 255


blackColor : Css.Color
blackColor =
    Css.rgb 0 0 0


boxStyle : View.Config -> List Css.Style
boxStyle _ =
    [ Css.backgroundColor defaultBackgroundColor
    , Css.boxShadow5 (Css.px 1) (Css.px 1) (Css.px 5) (Css.px 1) lighterGreyColor
    , Css.px 10 |> Css.padding
    ]


searchInputStyle : View.Config -> String -> List Css.Style
searchInputStyle _ _ =
    [ Css.pct 100 |> Css.width
    , Css.px 20 |> Css.height
    , Css.display Css.block
    , Css.color lightGreyColor
    , Css.border3 (Css.px 1) Css.solid lighterGreyColor
    , Css.backgroundColor defaultBackgroundColor
    , Css.px 3 |> Css.borderRadius
    , Css.px 25 |> Css.textIndent
    ]


propertyBoxHeading : View.Config -> List Css.Style
propertyBoxHeading _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.4 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


propertyBoxHeading2 : View.Config -> List Css.Style
propertyBoxHeading2 _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.3 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


toolItemStyle : View.Config -> List Css.Style
toolItemStyle _ =
    [ Css.px 55 |> Css.minWidth
    , Css.textAlign Css.center
    ]


linkButtonStyle : View.Config -> Bool -> List Css.Style
linkButtonStyle _ enabled =
    [ Css.backgroundColor defaultBackgroundColor, Css.px 0 |> Css.borderWidth, Css.cursor Css.pointer, Css.px 0 |> Css.padding, Css.px 5 |> Css.paddingLeft ]


toolButtonStyle : View.Config -> Bool -> List Css.Style
toolButtonStyle vc enabled =
    Css.textAlign Css.center :: linkButtonStyle vc enabled


toolIconStyle : View.Config -> List Css.Style
toolIconStyle _ =
    [ Css.em 1.3 |> Css.fontSize
    , Css.px 5 |> Css.marginBottom
    ]


topLeftPanelStyle : View.Config -> List Css.Style
topLeftPanelStyle _ =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.left
    , Css.px 10 |> Css.top
    ]


graphToolsStyle : View.Config -> List Css.Style
graphToolsStyle vc =
    [ Css.position Css.absolute
    , Css.pct 50 |> Css.left
    , Css.px 50 |> Css.bottom
    , Css.displayFlex
    , Css.transform (Css.translate (Css.pct -50))
    ]
        ++ boxStyle vc


topRightPanelStyle : View.Config -> List Css.Style
topRightPanelStyle _ =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.right
    , Css.px 10 |> Css.top
    ]


searchBoxStyle : View.Config -> List Css.Style
searchBoxStyle vc =
    [ Css.px 300 |> Css.minWidth
    , Css.px 10 |> Css.marginBottom
    ]
        ++ boxStyle vc


propertyBoxStyle : View.Config -> List Css.Style
propertyBoxStyle =
    searchBoxStyle


graphActionsViewStyle : View.Config -> List Css.Style
graphActionsViewStyle _ =
    [ Css.displayFlex, Css.justifyContent Css.flexEnd, Css.px 5 |> Css.margin ]


graphActionButtonStyle : View.Config -> Bool -> List Css.Style
graphActionButtonStyle _ enabled =
    [ Css.px 5 |> Css.margin
    , Css.cursor Css.pointer
    , Css.padding4 (Css.px 3) (Css.px 10) (Css.px 3) (Css.px 10)
    , Css.color lightGreyColor
    , Css.borderColor lighterGreyColor
    , Css.backgroundColor defaultBackgroundColor
    , Css.border3 (Css.px 1) Css.solid lighterGreyColor
    , Css.px 3 |> Css.borderRadius
    ]


searchViewStyle : View.Config -> List Css.Style
searchViewStyle vc =
    boxStyle vc ++ [ Css.displayFlex, Css.justifyContent Css.flexEnd ]


searchBoxContainerStyle : View.Config -> List Css.Style
searchBoxContainerStyle _ =
    [ Css.position Css.relative ]


searchBoxIconStyle : View.Config -> List Css.Style
searchBoxIconStyle _ =
    [ Css.position Css.absolute, Css.px 7 |> Css.top, Css.px 7 |> Css.left ]


propertyBoxActorImageStyle : View.Config -> List Css.Style
propertyBoxActorImageStyle _ =
    [ Css.display Css.block
    , Css.borderRadius (Css.pct 50)
    , Css.height (Css.px 40)
    , Css.width (Css.px 40)
    , Css.border3 (Css.px 1) Css.solid blackColor
    , Css.px 5 |> Css.marginRight
    ]


propertyBoxDetailsViewContainerStyle : View.Config -> List Css.Style
propertyBoxDetailsViewContainerStyle _ =
    [ Css.displayFlex, Css.justifyContent Css.left, Css.pct 100 |> Css.width ]



-- Config


type alias BtnConfig =
    { icon : FontAwesome.Icon, text : String, msg : Msg, enable : Bool }


graphTools : List BtnConfig
graphTools =
    [ BtnConfig FontAwesome.trash "restart" UserClickedRestart True
    , BtnConfig FontAwesome.redo "redo" UserClickedRedo False
    , BtnConfig FontAwesome.undo "undo" UserClickedUndo True
    , BtnConfig FontAwesome.highlighter "highlight" UserClickedHighlighter True
    ]


graphActionButtons : List BtnConfig
graphActionButtons =
    [ BtnConfig FontAwesome.arrowUp "Import file" UserClickedImportFile True
    , BtnConfig FontAwesome.arrowDown "Export graph" UserClickedExportGraph True
    ]



-- Helpers


disableableButton : (Bool -> List Css.Style) -> BtnConfig -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
disableableButton style btn attrs content =
    let
        addattr =
            if btn.enable then
                [ btn.msg |> onClick ]

            else
                [ HA.disabled True ]
    in
    button (([ style btn.enable |> HA.css ] ++ addattr) ++ attrs) content



-- View


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
    , topLeftPanel plugins states vc gc model
    , graphToolsView plugins states vc gc model
    , topRightPanel plugins states vc gc model
    ]


topLeftPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topLeftPanel plugins ms vc gc model =
    div [ topLeftPanelStyle vc |> HA.css ]
        [ h2 [ vc.theme.heading2 |> HA.css ] [ Html.text "Pathfinder" ]

        --, settingsView plugins ms vc gc model
        ]


settingsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
settingsView plugins _ vc gc model =
    div [ searchViewStyle vc |> HA.css ] [ Html.text "Display", FontAwesome.icon FontAwesome.chevronDown |> Html.fromUnstyled ]


graphToolsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
graphToolsView plugins _ vc gc model =
    div
        [ graphToolsStyle vc |> HA.css
        ]
        (graphTools |> List.map (graphToolButton vc))


graphToolButton : View.Config -> BtnConfig -> Svg Msg
graphToolButton vc btn =
    div [ toolItemStyle vc |> HA.css ]
        [ disableableButton (toolButtonStyle vc)
            btn
            []
            [ div [ toolIconStyle vc |> HA.css ] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]
            , Html.text (Locale.string vc.locale btn.text)
            ]
        ]


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topRightPanel plugins ms vc gc model =
    div [ topRightPanelStyle vc |> HA.css ]
        [ graphActionsView plugins ms vc gc model
        , searchBoxView plugins ms vc gc model
        , propertyBoxView plugins ms vc gc model
        ]


graphActionsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
graphActionsView plugins _ vc gc model =
    div [ graphActionsViewStyle vc |> HA.css ]
        (graphActionButtons |> List.map (graphActionButton vc))


graphActionButton : View.Config -> BtnConfig -> Html Msg
graphActionButton vc btn =
    disableableButton (graphActionButtonStyle vc) btn [] (iconWithText vc btn.icon (Locale.string vc.locale btn.text))


iconWithText : View.Config -> FontAwesome.Icon -> String -> List (Html Msg)
iconWithText _ faIcon text =
    [ span [ [ Css.px 5 |> Css.paddingRight ] |> HA.css ] [ FontAwesome.icon faIcon |> Html.fromUnstyled ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
searchBoxView plugins _ vc gc model =
    div
        [ searchBoxStyle vc |> HA.css ]
        [ h3 [ propertyBoxHeading2 vc |> HA.css ] [ Html.text (Locale.string vc.locale "Add to graph") ]
        , div [ searchBoxContainerStyle vc |> HA.css ]
            [ span [ searchBoxIconStyle vc |> HA.css ] [ FontAwesome.icon FontAwesome.search |> Html.fromUnstyled ]
            , View.Search.search plugins vc { css = searchInputStyle vc, multiline = False, resultsAsLink = True, showIcon = False } model.search |> Html.map SearchMsg
            ]
        ]


propertyBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
propertyBoxView plugins ms vc gc model =
    div
        [ propertyBoxStyle vc |> HA.css ]
        [ propertyBoxCloseRow vc
        , propertyBoxDetailsView plugins ms vc gc model
        ]


propertyBoxCloseRow : View.Config -> Html Msg
propertyBoxCloseRow vc =
    div [ [ Css.float Css.right ] |> HA.css ] [ closeButton vc UserClosedPropertyBox ]


closeButton : View.Config -> Msg -> Html Msg
closeButton vc msg =
    button [ linkButtonStyle vc True |> HA.css, msg |> onClick ] [ FontAwesome.icon FontAwesome.times |> Html.fromUnstyled ]


propertyBoxDetailsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
propertyBoxDetailsView plugins ms vc gc model =
    div [ propertyBoxDetailsViewContainerStyle vc |> HA.css ]
        [ img [ src (dummyImageSrc vc), propertyBoxActorImageStyle vc |> HA.css ] []
        , div [ [ Css.pct 100 |> Css.width ] |> HA.css ]
            [ itemHeadingAndLabelsView plugins ms vc gc model
            , hr [ [ Css.px 5 |> Css.marginBottom, Css.px 5 |> Css.marginTop ] |> HA.css ] []
            , itemDetailsView plugins ms vc gc model
            ]
        ]


itemHeadingAndLabelsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
itemHeadingAndLabelsView plugins ms vc gc model =
    let
        annotations =
            [ BtnConfig FontAwesome.tags "tags" NoOp True, BtnConfig FontAwesome.cog "is contract" NoOp True ]

        heading =
            "Bitcoin Address"

        identifier =
            "bc1qvqxjv6cdf9yxvv5yssujcvt8zu2qfl2nnuuy7d"
    in
    div []
        [ h1 [ propertyBoxHeading2 vc |> HA.css ] (Html.text (String.toUpper heading) :: (annotations |> List.map (annotationButton vc)))
        , copyableLongIdentifier vc [] identifier
        ]


annotationButton : View.Config -> BtnConfig -> Html Msg
annotationButton vc btn =
    disableableButton (linkButtonStyle vc) btn [] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]


itemDetailsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
itemDetailsView plugins ms vc gc model =
    div []
        []


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
