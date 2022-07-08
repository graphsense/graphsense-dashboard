module View.Graph.Link exposing (..)

import Color
import Config.Graph as Graph exposing (expandHandleWidth, linkLabelHeight, txMaxWidth)
import Config.View as View
import Css exposing (..)
import Css.Graph
import Init.Graph.Id as Id
import Json.Decode
import List.Extra
import Log
import Model.Currency as Currency
import Model.Graph exposing (NodeType)
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Link as Link exposing (Link)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Regex
import String.Interpolate
import Svg.Styled as S exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Util.View
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
    , selected : Bool
    , onMouseOver : Msg
    , onClick : Msg
    , nodeType : NodeType
    , color : Maybe Color.Color
    }


entityLinkOptions : View.Config -> Graph.Config -> String -> Entity -> Link Entity -> Options
entityLinkOptions vc gc selected entity link =
    { hovered = False
    , selected = selected == Id.entityLinkIdToString ( entity.id, link.node.id )
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
    , onClick = Id.initLinkId entity.id link.node.id |> UserClicksEntityLink
    , nodeType = Model.Graph.Entity
    , color =
        if entity.color /= Nothing && entity.color == link.node.color then
            entity.color

        else
            Nothing
    }


addressLinkOptions : View.Config -> Graph.Config -> String -> Address -> Link Address -> Options
addressLinkOptions vc gc selected address link =
    { hovered = False
    , selected = selected == Id.addressLinkIdToString ( address.id, link.node.id )
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
    , onClick = Id.initLinkId address.id link.node.id |> UserClicksAddressLink
    , nodeType = Model.Graph.Address
    , color =
        if address.color /= Nothing && address.color == link.node.color then
            address.color

        else
            Nothing
    }


entityLink : View.Config -> Graph.Config -> String -> Float -> Float -> Entity -> Link Entity -> Svg Msg
entityLink vc gc selected mn mx entity link =
    drawLink
        (entityLinkOptions vc gc selected entity link)
        vc
        gc
        mn
        mx


entityLinkHovered : View.Config -> Graph.Config -> Float -> Float -> Entity -> Link Entity -> Svg Msg
entityLinkHovered vc gc mn mx entity link =
    drawLink
        (entityLinkOptions vc gc "" entity link
            |> s_hovered True
        )
        vc
        gc
        mn
        mx


bezier : Float -> Float -> Float -> Float -> String
bezier =
    bezierInv False


bezierInv : Bool -> Float -> Float -> Float -> Float -> String
bezierInv inv sx sy tx ty =
    let
        cx =
            sx + (tx - sx) / 2

        templ =
            if inv then
                "M{3} {4}C{2} {4} {2} {1} {0} {1}"

            else
                "M{0} {1}C{2} {1} {2} {4} {3} {4}"
    in
    [ sx, sy, cx, tx, ty ]
        |> List.map String.fromFloat
        |> String.Interpolate.interpolate templ


shadowLink : View.Config -> Entity -> Link Entity -> Svg Msg
shadowLink vc entity link =
    let
        sx1 =
            Entity.getX entity + Entity.getWidth entity

        sy1 =
            Entity.getY entity

        tx1 =
            Entity.getX link.node

        ty1 =
            Entity.getY link.node

        sx2 =
            Entity.getX entity + Entity.getWidth entity

        sy2 =
            Entity.getY entity + Entity.getHeight entity

        tx2 =
            Entity.getX link.node

        ty2 =
            Entity.getY link.node + Entity.getHeight link.node

        th =
            Entity.getHeight link.node

        dd =
            [ sx1 -- 0
            , sy1 -- 1
            , sx1 + (tx1 - sx1) / 2 -- 2
            , tx1 -- 3
            , ty1 -- 4
            , th -- 5
            , sx2 -- 6
            , sy2 -- 7
            , sx2 + (tx2 - sx2) / 2 -- 8
            , tx2 -- 9
            , ty2 -- 10
            ]
                |> List.map String.fromFloat
                |> String.Interpolate.interpolate
                    "M{0} {1} {3} {4} {3} {10} {6} {7} Z"
                -- curved variant:
                --"M{0} {1}C{2} {1} {2} {4} {3} {4} V {10} C{8} {10} {8} {7} {6} {7} Z"
                |> d
    in
    S.path
        [ dd
        , Css.Graph.shadowLink vc |> css
        ]
        []


addressLink : View.Config -> Graph.Config -> String -> Float -> Float -> Address -> Link Address -> Svg Msg
addressLink vc gc selected mn mx address link =
    drawLink
        (addressLinkOptions vc gc selected address link)
        vc
        gc
        mn
        mx


addressLinkHovered : View.Config -> Graph.Config -> Float -> Float -> Address -> Link Address -> Svg Msg
addressLinkHovered vc gc mn mx address link =
    drawLink
        (addressLinkOptions vc gc "" address link
            |> s_hovered True
        )
        vc
        gc
        mn
        mx


drawLink : Options -> View.Config -> Graph.Config -> Float -> Float -> Svg Msg
drawLink { selected, color, hovered, sx, sy, tx, ty, amount, label, onMouseOver, onClick, nodeType } vc gc mn mx =
    let
        thickness =
            vc.theme.graph.linkThickness
                * (if mn == mx then
                    1

                   else
                    1 + (amount / mx) * txMaxWidth
                  )

        dd =
            bezier sx sy tx ty
                |> d

        lx =
            (sx + tx) / 2

        ly =
            (sy + ty) / 2 + Graph.linkLabelHeight / 3
    in
    g
        [ Svg.onMouseOver onMouseOver
        , Json.Decode.succeed ( onClick, True )
            |> Svg.stopPropagationOn "click"
        , onMouseOut UserLeavesThing
        ]
        [ S.path
            [ dd
            , Css.Graph.link vc nodeType hovered selected color
                ++ [ thickness
                        |> (\x -> String.fromFloat x ++ "px")
                        |> property "stroke-width"
                   , (if hovered then
                        vc.theme.graph.linkColorStrong

                      else if selected then
                        vc.theme.graph.linkColorSelected

                      else
                        color
                            |> Maybe.withDefault
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
        , drawLabel vc gc lx ly hovered selected color label
        ]


drawLabel : View.Config -> Graph.Config -> Float -> Float -> Bool -> Bool -> Maybe Color.Color -> String -> Svg Msg
drawLabel vc gc x y hovered selected color lbl =
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
            , Css.Graph.linkLabelBox vc hovered selected |> css
            , class "rectLabel"
            ]
            []
        , S.text_
            [ Css.Graph.linkLabel vc hovered selected color
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
    case link.link of
        Link.PlaceholderLinkData ->
            0

        Link.LinkData li ->
            case gc.txLabelType of
                Graph.NoTxs ->
                    li.noTxs
                        |> toFloat

                Graph.Value ->
                    Currency.valuesToFloat vc.locale.currency li.value
                        |> Maybe.withDefault 0


getLabel : View.Config -> Graph.Config -> String -> Link node -> String
getLabel vc gc currency link =
    case link.link of
        Link.PlaceholderLinkData ->
            ""

        Link.LinkData li ->
            case gc.txLabelType of
                Graph.NoTxs ->
                    Locale.int vc.locale li.noTxs

                Graph.Value ->
                    Locale.currencyWithoutCode vc.locale currency li.value
                        |> (++) "~"


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
        >> Regex.replace (Regex.fromString "[^a-z0-9]" |> Maybe.withDefault Regex.never) (\_ -> "")
        >> (++) "arrow"
