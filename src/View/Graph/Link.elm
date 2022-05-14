module View.Graph.Link exposing (..)

import Config.Graph as Graph exposing (expandHandleWidth, txMaxWidth)
import Config.View as View
import Css exposing (..)
import Css.Graph as Css
import List.Extra
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Link exposing (Link)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)


entityLink : View.Config -> Graph.Config -> Float -> Float -> Entity -> Link Entity -> Svg Msg
entityLink vc gc mn mx entity link =
    let
        sx =
            Entity.getX entity + Entity.getWidth entity + expandHandleWidth

        sy =
            Entity.getY entity + Entity.getHeight entity / 2

        tx =
            Entity.getX link.node - expandHandleWidth

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
    in
    g []
        [ Svg.path
            [ dd
            , Css.entityLink vc
                ++ [ thickness
                        |> (\x -> String.fromFloat x ++ "px")
                        |> property "stroke-width"
                   ]
                |> css
            ]
            []
        , Svg.path
            [ dd
            , [ Basics.min 6 thickness
                    |> (\x -> String.fromFloat x ++ "px")
                    |> Css.property "stroke-width"
              , Css.opacity (int 0)
              ]
                |> css
            ]
            []
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
