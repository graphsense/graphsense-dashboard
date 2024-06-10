module View.Pathfinder.Tx.Utxo exposing (edge, view)

import Api.Data
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Dict exposing (Dict)
import Dict.Nonempty as NDict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.PathD exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple exposing (pair, second)
import Util.Graph exposing (translate)
import Util.Pathfinder exposing (getAddress)
import View.Locale as Locale


view : Plugins -> View.Config -> Pathfinder.Config -> Id -> UtxoTx -> Svg Msg
view _ vc _ id tx =
    let
        unit =
            View.getUnit vc

        anyIsNotVisible =
            NDict.toList
                >> List.any (second >> .visible >> not)
    in
    body vc
        :: (if anyIsNotVisible tx.inputs || anyIsNotVisible tx.outputs then
                [ moreIndicator vc
                ]

            else
                []
           )
        |> g
            [ translate (tx.x * unit) (tx.y * unit)
                |> transform
            , Css.tx vc |> css
            , UserClickedTx id |> onClick
            ]


moreIndicator : View.Config -> Svg Msg
moreIndicator vc =
    let
        dot attrs =
            circle
                (attrs
                    ++ [ vc.theme.pathfinder.txRadius
                            / 3
                            |> String.fromFloat
                            |> r
                       ]
                )
                []
    in
    g
        [ translate 0 (vc.theme.pathfinder.txRadius * 1.5)
            |> transform
        ]
        [ dot
            [ cx "0"
            , cy "0"
            ]
        , dot
            [ cy "0"
            , vc.theme.pathfinder.txRadius
                / 1.5
                |> String.fromFloat
                |> cx
            ]
        , dot
            [ cy "0"
            , -vc.theme.pathfinder.txRadius
                / 1.5
                |> String.fromFloat
                |> cx
            ]
        ]


body : View.Config -> Svg Msg
body vc =
    circle
        [ cx "0"
        , cy "0"
        , r <| String.fromFloat vc.theme.pathfinder.txRadius
        ]
        []


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> UtxoTx -> Svg Msg
edge _ vc gc addresses tx =
    let
        toValues =
            NDict.toList
                >> List.filterMap
                    (\( id, { values } ) ->
                        getAddress addresses id
                            |> Result.toMaybe
                            |> Maybe.map (pair values)
                    )

        outputValues =
            tx.outputs
                |> toValues

        inputValues =
            tx.inputs
                |> toValues
    in
    (outputValues
        |> List.map
            (\( values, address ) ->
                ( Id.toString address.id
                , Svg.lazy7 outPath
                    vc
                    gc
                    values
                    tx.x
                    tx.y
                    (address.x + address.dx)
                    (address.y + address.dy)
                )
            )
    )
        ++ (inputValues
                |> List.map
                    (\( values, address ) ->
                        ( Id.toString address.id
                        , Svg.lazy7 inPath
                            vc
                            gc
                            values
                            tx.x
                            tx.y
                            (address.x + address.dx)
                            (address.y + address.dy)
                        )
                    )
           )
        |> Keyed.node "g" []


outPath : View.Config -> Pathfinder.Config -> Api.Data.Values -> Float -> Float -> Float -> Float -> Svg Msg
outPath vc gc value tx ty ax ay =
    let
        unit =
            View.getUnit vc

        rad =
            vc.theme.pathfinder.addressRadius

        txRad =
            vc.theme.pathfinder.txRadius

        x1 =
            tx
                * unit
                + txRad
                * (if ax < tx then
                    -1

                   else
                    1
                  )

        y1 =
            ty * unit

        x2 =
            ax
                * unit
                - rad
                * (if ax < tx then
                    -1

                   else
                    1
                  )

        y2 =
            ay * unit
    in
    coloredPath vc gc value True x1 y1 x2 y2


inPath : View.Config -> Pathfinder.Config -> Api.Data.Values -> Float -> Float -> Float -> Float -> Svg Msg
inPath vc gc value tx ty ax ay =
    let
        unit =
            View.getUnit vc

        rad =
            vc.theme.pathfinder.addressRadius

        txRad =
            vc.theme.pathfinder.txRadius

        x1 =
            ax
                * unit
                + rad
                * (if tx < ax then
                    -1

                   else
                    1
                  )

        y1 =
            ay * unit

        x2 =
            tx
                * unit
                - txRad
                * (if tx < ax then
                    -1

                   else
                    1
                  )

        y2 =
            ty * unit
    in
    coloredPath vc gc value False x1 y1 x2 y2


valueToLabel : View.Config -> Api.Data.Values -> String
valueToLabel vc value =
    Locale.currency vc.locale [ ( { network = "btc", asset = "btc" }, value ) ]


coloredPath : View.Config -> Pathfinder.Config -> Api.Data.Values -> Bool -> Float -> Float -> Float -> Float -> Svg Msg
coloredPath vc _ value outgoing x1 y1 x2_ y2_ =
    let
        x2 =
            if x1 == x2_ then
                x2_ + 0.01

            else
                x2_

        y2 =
            if y1 == y2_ then
                y2_ + 0.01

            else
                y2_

        ( dx, dy ) =
            ( x2 - x1
            , y2 - y1
            )

        ( nodes, lx, ly ) =
            let
                ( mx, my ) =
                    ( x1 + dx / 2
                    , y1 + dy / 2
                    )
            in
            ( [ ( x2, y2 )
                    |> C
                        ( mx, y1 )
                        ( mx, y2 )
              ]
            , mx
            , my
            )

        label =
            valueToLabel vc value
    in
    [ Svg.path
        [ nodes
            |> (::) (M ( x1, y1 ))
            |> pathD
            |> d
        , css
            [ Css.property "stroke" <|
                "url(#"
                    ++ (case ( outgoing, dx > 0 ) of
                            ( True, True ) ->
                                "outEdge"

                            ( True, False ) ->
                                "outEdgeBack"

                            ( False, True ) ->
                                "inEdge"

                            ( False, False ) ->
                                "inEdgeBack"
                       )
                    ++ ")"
            , Css.property "fill" "none"
            , Css.property "stroke-width" "2px"
            ]
        ]
        []
    , if outgoing then
        let
            arrowLength =
                vc.theme.pathfinder.arrowLength
        in
        Svg.path
            [ d <|
                pathD
                    [ M ( x2 - arrowLength, y2 - arrowLength )
                    , l ( arrowLength, arrowLength )
                    , l ( -arrowLength, arrowLength )
                    ]
            , "rotate("
                ++ ([ if dx < 0 then
                        "180"

                      else
                        "0"
                    , String.fromFloat x2
                    , String.fromFloat y2
                    ]
                        |> String.join ","
                   )
                ++ ")"
                |> transform
            , css
                (Css.edgeUtxo vc
                    ++ [ Css.property "stroke" vc.theme.pathfinder.outEdgeColor ]
                )
            ]
            []

      else
        text ""
    , text_
        [ lx |> String.fromFloat |> x
        , ly |> String.fromFloat |> y
        , textAnchor "middle"
        , Css.edgeLabel vc |> css
        ]
        [ text label
        ]
    ]
        |> g []


bendedPath : View.Config -> Pathfinder.Config -> Api.Data.Values -> Bool -> Float -> Float -> Float -> Float -> Svg Msg
bendedPath vc _ value withArrow x1 y1 x2 y2 =
    let
        ( dx, dy ) =
            ( x2 - x1
            , y2 - y1
            )

        ( nodes, lx, ly ) =
            if dx > 0 then
                let
                    ( mx, my ) =
                        ( x1 + dx / 2
                        , y1 + dy / 2
                        )
                in
                ( [ ( x2, y2 )
                        |> C
                            ( mx, y1 )
                            ( mx, y2 )
                  ]
                , mx
                , my
                )

            else
                let
                    ( mx, my ) =
                        ( x1 + dx / 2
                        , y1 + Basics.max (dy / 2) vc.theme.pathfinder.addressRadius
                        )

                    ( c1x, c1y ) =
                        ( x1 + (mx - x1 |> abs) / 2
                        , my
                        )

                    ( c2x, c2y ) =
                        ( mx
                            - (mx - x1)
                            / 2
                        , my
                        )
                in
                ( [ ( mx, my )
                        |> C
                            ( c1x, c1y )
                            ( c2x, c2y )
                  , ( x2, y2 )
                        |> S
                            ( x2 - (x2 - mx |> abs) / 2
                            , y2 - (y2 - my) / 2
                            )
                  ]
                , mx
                , my
                )

        label =
            valueToLabel vc value
    in
    [ Svg.path
        [ nodes
            |> (::) (M ( x1, y1 ))
            |> pathD
            |> d
        , Css.edgeUtxo vc |> css
        ]
        []
    , if withArrow then
        let
            arrowLength =
                vc.theme.pathfinder.arrowLength
        in
        Svg.path
            [ d <|
                pathD
                    [ M ( x2 - arrowLength, y2 - arrowLength )
                    , l ( arrowLength, arrowLength )
                    , l ( -arrowLength, arrowLength )
                    ]
            , Css.edgeUtxo vc |> css
            ]
            []

      else
        text ""
    , text_
        [ lx |> String.fromFloat |> x
        , ly |> String.fromFloat |> y
        , textAnchor "middle"
        , Css.edgeLabel vc |> css
        ]
        [ text label
        ]
    ]
        |> g []
