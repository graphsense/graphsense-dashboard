module View.Graph.Label exposing (label, normalizeValues, split)

import Api.Data
import Basics.Extra exposing (uncurry)
import Config.Graph as Graph exposing (labelHeight)
import Config.View exposing (Config)
import Css exposing (..)
import Css.Graph as Css
import Dict exposing (Dict)
import List.Extra
import Model.Graph
import Msg.Graph exposing (Msg)
import String.Extra
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Tuple exposing (mapSecond)
import Util.Graph as Util exposing (filterTxValue)
import View.Locale as Locale


label : Config -> Graph.Config -> Model.Graph.NodeType -> String -> Svg Msg
label vc gc nodeType title =
    let
        ( dyOffset, linespacing ) =
            case nodeType of
                Model.Graph.AddressType ->
                    ( 0.36, 0.8 )

                Model.Graph.EntityType ->
                    ( 0.75, 0.2 )
    in
    if title == "tag locked" then
        let
            offset =
                String.fromInt <|
                    case nodeType of
                        Model.Graph.AddressType ->
                            -10

                        Model.Graph.EntityType ->
                            -2
        in
        g []
            [ Svg.Styled.path
                [ (Css.property "transform" ("translateY(" ++ offset ++ "px) scale(0.03)")
                    :: Css.tagLockedIcon vc
                  )
                    |> css
                , d "M80 192V144C80 64.47 144.5 0 224 0C303.5 0 368 64.47 368 144V192H384C419.3 192 448 220.7 448 256V448C448 483.3 419.3 512 384 512H64C28.65 512 0 483.3 0 448V256C0 220.7 28.65 192 64 192H80zM144 192H304V144C304 99.82 268.2 64 224 64C179.8 64 144 99.82 144 144V192z"
                ]
                []
            , Svg.Styled.text_
                [ Util.translate 17 0 |> Svg.transform
                , String.fromFloat dyOffset ++ "em" |> Svg.dy
                , (px (labelHeight * 0.9) |> Css.fontSize)
                    :: Css.labelText vc nodeType
                    ++ Css.tagLockedText vc
                    |> css
                ]
                [ Locale.string vc.locale "proprietary tag" |> text
                ]
            ]

    else
        let
            lbl =
                String.left (gc.maxLettersPerLabelRow * 2) title

            maxLettersBeforeResize =
                18

            height =
                labelHeight
                    * (if String.length lbl > maxLettersBeforeResize then
                        0.8

                       else
                        1
                      )

            spl =
                split gc.maxLettersPerLabelRow lbl
        in
        spl
            |> List.indexedMap
                (\i row ->
                    tspan
                        [ x "0"
                        , (toFloat i * linespacing + dyOffset |> String.fromFloat) ++ "em" |> Svg.dy
                        ]
                        [ text row
                        ]
                )
            |> Svg.Styled.text_
                [ ((px height |> Css.fontSize)
                    :: Css.labelText vc nodeType
                  )
                    |> css
                ]


split : Int -> String -> List String
split maxLettersPerRow string =
    String.split " " string
        |> List.foldl
            (\word ( current, more ) ->
                (current
                    ++ (if String.isEmpty current then
                            ""

                        else
                            " "
                       )
                    ++ word
                )
                    |> String.Extra.break maxLettersPerRow
                    |> List.Extra.unconsLast
                    |> Maybe.map (mapSecond ((++) more))
                    |> Maybe.withDefault ( current, more )
            )
            ( "", [] )
        |> (\( current, more ) -> more ++ [ current ])
        |> List.map String.trim


normalizeValues : Graph.Config -> String -> Api.Data.Values -> Maybe (Dict String Api.Data.Values) -> List ( String, Api.Data.Values )
normalizeValues gc parentCurrency value tokenValues =
    ( parentCurrency, value )
        :: (tokenValues
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []
           )
        |> List.filter (uncurry (filterTxValue gc))
