module View.Graph.Link exposing (..)

import Color exposing (Color)
import Config.Graph as Graph exposing (expandHandleWidth, linkLabelHeight, txMaxWidth)
import Config.View as View
import Css exposing (..)
import Css.Graph
import Init.Graph.Id as Id
import List.Extra
import Log
import Model.Graph exposing (NodeType)
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Link exposing (Link)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Regex
import String.Interpolate
import Svg.Styled as S exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import View.Locale as Locale


linkPrefix : String
linkPrefix =
    "link"


type alias Options =
    { sx : Float
    , sy : Float
    , tx : Float
    , ty : Float
    , label : String
    , amount : Float
    , hovered : Bool
    , onMouseOver : Msg
    , nodeType : NodeType
    }


entityLinkOptions : View.Config -> Graph.Config -> Entity -> Link Entity -> Options
entityLinkOptions vc gc entity link =
    { hovered = False
    , sx = Entity.getX entity + Entity.getWidth entity
    , sy =
        Entity.getY entity + Entity.getHeight entity / 2
    , tx =
        Entity.getX link.node - Graph.arrowHeight
    , ty =
        Entity.getY link.node + Entity.getHeight link.node / 2
    , amount = getLinkAmount vc gc link
    , label =
        getLabel vc gc link.node.entity.currency link
    , onMouseOver = Id.initLinkId entity.id link.node.id |> UserHoversEntityLink
    , nodeType = Model.Graph.Entity
    }


addressLinkOptions : View.Config -> Graph.Config -> Address -> Link Address -> Options
addressLinkOptions vc gc address link =
    { hovered = False
    , sx = Address.getX address + Address.getWidth address
    , sy =
        Address.getY address + Address.getHeight address / 2
    , tx =
        Address.getX link.node - Graph.arrowHeight
    , ty =
        Address.getY link.node + Address.getHeight link.node / 2
    , amount = getLinkAmount vc gc link
    , label =
        getLabel vc gc link.node.address.currency link
    , onMouseOver = Id.initLinkId address.id link.node.id |> UserHoversAddressLink
    , nodeType = Model.Graph.Address
    }


entityLink : View.Config -> Graph.Config -> Float -> Float -> Entity -> Link Entity -> Svg Msg
entityLink vc gc mn mx entity link =
    drawLink
        (entityLinkOptions vc gc entity link)
        vc
        gc
        mn
        mx


entityLinkHovered : View.Config -> Graph.Config -> Float -> Float -> Entity -> Link Entity -> Svg Msg
entityLinkHovered vc gc mn mx entity link =
    drawLink
        (entityLinkOptions vc gc entity link
            |> s_hovered True
        )
        vc
        gc
        mn
        mx


addressLink : View.Config -> Graph.Config -> Float -> Float -> Address -> Link Address -> Svg Msg
addressLink vc gc mn mx address link =
    drawLink
        (addressLinkOptions vc gc address link)
        vc
        gc
        mn
        mx


addressLinkHovered : View.Config -> Graph.Config -> Float -> Float -> Address -> Link Address -> Svg Msg
addressLinkHovered vc gc mn mx address link =
    drawLink
        (addressLinkOptions vc gc address link
            |> s_hovered True
        )
        vc
        gc
        mn
        mx


drawLink : Options -> View.Config -> Graph.Config -> Float -> Float -> Svg Msg
drawLink { hovered, sx, sy, tx, ty, amount, label, onMouseOver, nodeType } vc gc mn mx =
    let
        cx =
            sx + (tx - sx) / 2

        thickness =
            vc.theme.graph.linkThickness
                * (if mn == mx then
                    1

                   else
                    1 + (amount / mx) * txMaxWidth
                  )

        dd =
            [ sx, sy, cx, tx, ty ]
                |> List.map String.fromFloat
                |> String.Interpolate.interpolate
                    "M{0} {1}C{2} {1} {2} {4} {3} {4}"
                |> d

        lx =
            (sx + tx) / 2

        ly =
            (sy + ty) / 2 + Graph.linkLabelHeight / 3
    in
    g
        [ Svg.onMouseOver onMouseOver
        , onMouseOut UserLeavesThing
        ]
        [ S.path
            [ dd
            , Css.Graph.link vc nodeType hovered
                ++ [ thickness
                        |> (\x -> String.fromFloat x ++ "px")
                        |> property "stroke-width"
                   , (if hovered then
                        vc.theme.graph.linkColorStrong

                      else
                        vc.theme.graph.linkColorFaded
                     )
                        |> arrowMarkerId
                        |> List.singleton
                        |> String.Interpolate.interpolate
                            "url(#{0})"
                        |> property "marker-end"
                   ]
                |> css
            ]
            []
        , S.path
            [ dd
            , [ Basics.max 8 thickness
                    |> (\x -> String.fromFloat x ++ "px")
                    |> Css.property "stroke-width"
              , Css.opacity (int 0)
              , Css.property "fill" "none"
              , Css.property "stroke" "transparent"
              , Css.cursor Css.pointer
              ]
                |> css
            ]
            []
        , drawLabel vc gc lx ly hovered label
        ]


drawLabel : View.Config -> Graph.Config -> Float -> Float -> Bool -> String -> Svg Msg
drawLabel vc gc x y hovered lbl =
    let
        width =
            toFloat (String.length lbl) * linkLabelHeight / 1.5

        height =
            linkLabelHeight * 1.2
    in
    g
        []
        [ rect
            [ String.fromFloat (linkLabelHeight / 2) |> rx
            , String.fromFloat (linkLabelHeight / 2) |> ry
            , x - width / 2 |> String.fromFloat |> Svg.x
            , y - height * 0.85 |> String.fromFloat |> Svg.y
            , String.fromFloat width |> Svg.width
            , String.fromFloat height |> Svg.height
            , Css.Graph.linkLabelBox vc hovered |> css
            ]
            []
        , S.text_
            [ Css.Graph.linkLabel vc hovered
                |> css
            , textAnchor "middle"
            , String.fromFloat x |> Svg.x
            , String.fromFloat y |> Svg.y
            ]
            [ text lbl
            ]
        ]


getLinkAmount : View.Config -> Graph.Config -> Link node -> Float
getLinkAmount vc gc link =
    case gc.txLabelType of
        Graph.NoTxs ->
            link.noTxs
                |> toFloat

        Graph.Value ->
            case vc.locale.currency of
                Locale.Coin ->
                    link.value.value
                        |> toFloat

                Locale.Fiat curr ->
                    List.Extra.find (.code >> (==) curr) link.value.fiatValues
                        |> Maybe.map .value
                        |> Maybe.withDefault 0


getLabel : View.Config -> Graph.Config -> String -> Link node -> String
getLabel vc gc currency link =
    case gc.txLabelType of
        Graph.NoTxs ->
            Locale.int vc.locale link.noTxs

        Graph.Value ->
            Locale.currencyWithoutCode vc.locale currency link.value


arrowMarker : View.Config -> Graph.Config -> Color.Color -> Svg Msg
arrowMarker vc gc color =
    marker
        [ arrowMarkerId color |> id
        , String.fromFloat Graph.arrowWidth |> markerWidth
        , String.fromFloat Graph.arrowHeight |> markerHeight
        , refX "0"
        , Graph.arrowHeight / 2 |> String.fromFloat |> refY
        , orient "auto"
        , markerUnits "userSpaceOnUse"
        ]
        [ S.path
            [ [ Graph.arrowHeight
              , Graph.arrowWidth
              , Graph.arrowHeight / 2
              ]
                |> List.map String.fromFloat
                |> String.Interpolate.interpolate
                    "M0,0 L0,{0} L{1},{2} Z"
                |> d
            , Color.toCssString color
                |> property "fill"
                |> List.singleton
                |> css
            ]
            []
        ]


arrowMarkerId : Color.Color -> String
arrowMarkerId =
    Color.toCssString
        >> Log.log "before replaced"
        >> Regex.replace (Regex.fromString "[^a-z0-9]" |> Maybe.withDefault Regex.never) (\_ -> "")
        >> Log.log "replaced"
        >> (++) "arrow"
