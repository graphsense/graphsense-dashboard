module View.Graph.Label exposing (label, split)

import Config.Graph as Graph exposing (addressesCountHeight, labelHeight)
import Config.View exposing (Config)
import Css exposing (..)
import List.Extra
import Msg.Graph exposing (Msg(..))
import String.Extra
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events exposing (..)
import Tuple exposing (mapSecond)
import Util.Graph as Util exposing (translate)


label : Config -> Graph.Config -> String -> Svg Msg
label vc gc title =
    let
        lbl =
            String.left (gc.maxLettersPerLabelRow * 2) title

        ll =
            String.length lbl

        height =
            labelHeight
                * 1.3
                * (if ll > gc.maxLettersPerLabelRow then
                    0.5

                   else
                    1
                  )

        spl =
            split gc.maxLettersPerLabelRow lbl

        dy =
            toFloat (List.length spl - 1) * labelHeight / 2.5 |> negate
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
        |> Svg.text_
            [ Util.translate 0 dy |> Svg.transform
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
