module View.Graph.Link exposing (..)

import Color exposing (Color)
import Config.Graph as Graph exposing (expandHandleWidth, linkLabelHeight, txMaxWidth)
import Config.View as View
import Css exposing (..)
import Css.Graph as Css
import Init.Graph.Id as Id
import List.Extra
import Log
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Link exposing (Link)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import Regex
import String.Interpolate
import Svg.Styled as S exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events exposing (..)
import View.Locale as Locale


events : Id.EntityId -> Id.EntityId -> List (S.Attribute Msg)
events src tgt =
    [ Id.initEntityLinkId src tgt
        |> UserHoversEntityLink
        |> onMouseOver
    , onMouseOut UserLeavesThing
    ]


linkPrefix : String
linkPrefix =
    "link"


linkId2 : Id.EntityId -> Id.EntityId -> String
linkId2 src tgt =
    (Id.initEntityLinkId src tgt
        |> Id.entityLinkIdToString
    )
        |> (++) linkPrefix


linkId : Id.LinkId Id.EntityId -> String
linkId id =
    linkPrefix ++ Id.entityLinkIdToString id


entityLink : View.Config -> Graph.Config -> Float -> Float -> Id.LinkId Id.EntityId -> Entity -> Link Entity -> Svg Msg
entityLink vc gc mn mx hoveredLink entity link =
    let
        sx =
            Entity.getX entity + Entity.getWidth entity

        sy =
            Entity.getY entity + Entity.getHeight entity / 2

        tx =
            Entity.getX link.node - Graph.arrowHeight

        ty =
            Entity.getY link.node + Entity.getHeight link.node / 2

        cx =
            sx + (tx - sx) / 2

        thickness =
            vc.theme.graph.entityLinkThickness
                * (if mn == mx then
                    1

                   else
                    1 + (getLinkAmount vc gc link / mx) * txMaxWidth
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

        linkId_ =
            Id.initEntityLinkId entity.id link.node.id

        hovered =
            hoveredLink == linkId_
    in
    g
        (Svg.id (linkId linkId_) :: events entity.id link.node.id)
        [ S.path
            [ dd
            , Css.entityLink vc hovered
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
        , label vc gc lx ly hovered entity link
        ]


label : View.Config -> Graph.Config -> Float -> Float -> Bool -> Entity -> Link Entity -> Svg Msg
label vc gc x y hovered entity link =
    let
        lbl =
            getLabel vc gc link

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
            , Css.linkLabelBox vc hovered |> css
            ]
            []
        , S.text_
            [ Css.linkLabel vc hovered
                |> css
            , textAnchor "middle"
            , String.fromFloat x |> Svg.x
            , String.fromFloat y |> Svg.y
            ]
            [ text lbl
            ]
        ]


getLinkAmount : View.Config -> Graph.Config -> Link Entity -> Float
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


getLabel : View.Config -> Graph.Config -> Link Entity -> String
getLabel vc gc link =
    case gc.txLabelType of
        Graph.NoTxs ->
            Locale.int vc.locale link.noTxs

        Graph.Value ->
            Locale.currencyWithoutCode vc.locale link.node.entity.currency link.value


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
