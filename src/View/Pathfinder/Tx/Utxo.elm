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
import Tuple exposing (first, pair)
import Util.Graph exposing (translate)
import Util.Pathfinder exposing (getAddress)
import View.Locale as Locale


view : Plugins -> View.Config -> Pathfinder.Config -> Id -> UtxoTx -> Svg Msg
view _ vc _ id tx =
    let
        _ =
            Debug.log "Utxo.view" id

        unit =
            View.getUnit vc
    in
    [ body vc
    ]
        |> g
            [ translate (tx.x * unit) (tx.y * unit)
                |> transform
            , Css.tx vc |> css
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
edge plugins vc gc addresses tx =
    let
        toValues =
            NDict.toList
                >> List.filterMap
                    (\( id, values ) ->
                        getAddress addresses id
                            |> Result.toMaybe
                            |> Maybe.map (pair values)
                    )
    in
    (tx.outputs
        |> toValues
        |> List.map
            (\( values, address ) ->
                ( Id.toString address.id
                , Svg.lazy7 outPath
                    vc
                    gc
                    values
                    tx.x
                    tx.y
                    address.x
                    address.y
                )
            )
    )
        ++ (tx.inputs
                |> toValues
                |> List.map
                    (\( values, address ) ->
                        ( Id.toString address.id
                        , Svg.lazy7 inPath
                            vc
                            gc
                            values
                            tx.x
                            tx.y
                            address.x
                            address.y
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
            tx * unit + txRad

        y1 =
            ty * unit

        x2 =
            ax * unit - rad

        y2 =
            ay * unit
    in
    path vc gc value True x1 y1 x2 y2


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
            ax * unit + rad

        y1 =
            ay * unit

        x2 =
            tx * unit - txRad

        y2 =
            ty * unit
    in
    path vc gc value False x1 y1 x2 y2


valueToLabel : View.Config -> Api.Data.Values -> String
valueToLabel vc value =
    Locale.currency vc.locale [ ( { network = "btc", asset = "btc" }, value ) ]


path : View.Config -> Pathfinder.Config -> Api.Data.Values -> Bool -> Float -> Float -> Float -> Float -> Svg Msg
path vc gc value withArrow x1 y1 x2 y2 =
    let
        dx =
            x2 - x1

        label =
            valueToLabel vc value

        ( lx, ly, ta ) =
            if withArrow then
                ( x2, y2, "end" )

            else
                ( x1, y1, "start" )
    in
    [ Svg.path
        [ d <|
            pathD
                [ M ( x1, y1 )
                , ( x1 + dx * vc.theme.pathfinder.txEdgeCurvedEnd, y2 )
                    |> C
                        ( x1, y2 )
                        ( x1, y2 )
                , L ( x2, y2 )
                ]
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
        [ x <| String.fromFloat lx
        , y <| String.fromFloat ly
        , textAnchor ta
        ]
        [ text label
        ]
    ]
        |> g []
