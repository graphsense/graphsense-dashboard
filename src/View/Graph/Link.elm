module View.Graph.Link exposing (..)

import Color exposing (Color)
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
    }


entityLinkOptions : View.Config -> Graph.Config -> Entity -> Link Entity -> Options
entityLinkOptions vc gc entity link =
    { hovered = False
    , selected = False
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
    , onClick = NoOp
    , nodeType = Model.Graph.Entity
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
drawLink { selected, hovered, sx, sy, tx, ty, amount, label, onMouseOver, onClick, nodeType } vc gc mn mx =
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
        , Json.Decode.succeed ( onClick, True )
            |> Svg.stopPropagationOn "click"
        , onMouseOut UserLeavesThing
        ]
        [ S.path
            [ dd
            , Css.Graph.link vc nodeType hovered selected
                ++ [ thickness
                        |> (\x -> String.fromFloat x ++ "px")
                        |> property "stroke-width"
                   , (if hovered then
                        vc.theme.graph.linkColorStrong

                      else if selected then
                        vc.theme.graph.linkColorSelected

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
        , drawLabel vc gc lx ly hovered selected label
        ]


drawLabel : View.Config -> Graph.Config -> Float -> Float -> Bool -> Bool -> String -> Svg Msg
drawLabel vc gc x y hovered selected lbl =
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
            [ Css.Graph.linkLabel vc hovered selected
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
