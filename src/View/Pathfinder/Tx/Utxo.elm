module View.Pathfinder.Tx.Utxo exposing (edge, view)

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
import Tuple exposing (first)
import Util.Graph exposing (translate)
import Util.Pathfinder exposing (getAddress)


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
        unit =
            View.getUnit vc

        rad =
            vc.theme.pathfinder.addressRadius

        txRad =
            vc.theme.pathfinder.txRadius
    in
    (tx.outputs
        |> NDict.toList
        |> List.filterMap (first >> getAddress addresses >> Result.toMaybe)
        |> List.map
            (\address ->
                ( Id.toString address.id
                , Svg.lazy7 path
                    vc
                    gc
                    True
                    (tx.x * unit + txRad)
                    (tx.y * unit)
                    (address.x * unit - rad)
                    (address.y * unit)
                )
            )
    )
        ++ (tx.inputs
                |> NDict.toList
                |> List.filterMap (first >> getAddress addresses >> Result.toMaybe)
                |> List.map
                    (\address ->
                        ( Id.toString address.id
                        , Svg.lazy7 path
                            vc
                            gc
                            False
                            (address.x * unit + rad)
                            (address.y * unit)
                            (tx.x * unit - txRad)
                            (tx.y * unit)
                        )
                    )
           )
        |> Keyed.node "g" []


path : View.Config -> Pathfinder.Config -> Bool -> Float -> Float -> Float -> Float -> Svg Msg
path vc gc withArrow x1 y1 x2 y2 =
    let
        dx =
            x2 - x1
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
    ]
        |> g []
