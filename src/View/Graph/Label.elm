module View.Graph.Label exposing (label, split)

import Config.Graph as Graph exposing (addressesCountHeight, labelHeight)
import Config.View exposing (Config)
import Css exposing (..)
import Css.Graph as Css
import List.Extra
import Model.Graph
import Msg.Graph exposing (Msg(..))
import String.Extra
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events exposing (..)
import Tuple exposing (mapSecond)
import Util.Graph as Util exposing (translate)
import Util.View exposing (truncate)
import View.Locale as Locale


label : Config -> Graph.Config -> Model.Graph.NodeType -> String -> Svg Msg
label vc gc nodeType title =
    if title == "tag locked" then
        let
            offset =
                String.fromInt <|
                    case nodeType of
                        Model.Graph.Address ->
                            10

                        Model.Graph.Entity ->
                            0
        in
        g []
            [ Svg.Styled.path
                [ (Css.property "transform" ("translateY(-" ++ offset ++ "px) scale(0.03)")
                    :: Css.tagLockedIcon vc
                  )
                    |> css
                , d "M80 192V144C80 64.47 144.5 0 224 0C303.5 0 368 64.47 368 144V192H384C419.3 192 448 220.7 448 256V448C448 483.3 419.3 512 384 512H64C28.65 512 0 483.3 0 448V256C0 220.7 28.65 192 64 192H80zM144 192H304V144C304 99.82 268.2 64 224 64C179.8 64 144 99.82 144 144V192z"
                ]
                []
            , Svg.Styled.text_
                [ Util.translate 17 0 |> Svg.transform
                , (px labelHeight |> Css.fontSize)
                    :: Css.labelText vc nodeType
                    ++ Css.tagLockedText vc
                    |> css
                ]
                [ Locale.string vc.locale "tag locked" |> text
                ]
            ]

    else
        let
            lbl =
                String.left (gc.maxLettersPerLabelRow * 2) title

            ll =
                String.length lbl

            maxLettersBeforeResize =
                15

            height =
                labelHeight
                    * (if String.length lbl > maxLettersBeforeResize then
                        0.8

                       else
                        1
                      )

            spl =
                split gc.maxLettersPerLabelRow lbl

            dy =
                toFloat (List.length spl) * height / 5 |> negate
        in
        spl
            |> List.indexedMap
                (\i row ->
                    tspan
                        [ x "0"
                        , (toFloat i * 1.2 |> String.fromFloat) ++ "em" |> Svg.dy
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
