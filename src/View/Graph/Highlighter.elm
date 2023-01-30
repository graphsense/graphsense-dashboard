module View.Graph.Highlighter exposing (..)

import Color exposing (Color)
import Config.View as View
import Css
import Css.Graph as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Model.Graph.Highlighter exposing (..)
import Msg.Graph exposing (Msg(..))
import Tuple exposing (..)
import Util.View


tool : View.Config -> Model -> List (Html Msg)
tool vc model =
    div
        [ Css.highlightsRoot vc |> css
        ]
        [ div
            [ Css.highlightsColors vc |> css
            ]
            (vc.theme.graph.highlightsColorScheme
                |> List.filter
                    (\color ->
                        List.any
                            (second >> Color.toCssString >> (==) (Color.toCssString color))
                            model.highlights
                            |> not
                    )
                |> List.map
                    (\color ->
                        span
                            [ (Css.color (Util.View.toCssColor color)
                                :: Css.highlightsColor vc
                              )
                                |> css
                            , color
                                |> UserClickedHighlightColor
                                |> onClick
                            ]
                            [ square
                            ]
                    )
            )
        , div
            [ Css.highlights vc |> css
            ]
            (model.highlights
                |> List.indexedMap (viewHighlight vc model.selected)
            )
        ]
        |> List.singleton


viewHighlight : View.Config -> Maybe Int -> Int -> ( String, Color ) -> Html Msg
viewHighlight vc selected i ( title, color ) =
    div
        [ Css.highlightRoot vc |> css
        ]
        [ span
            [ (Css.color (Util.View.toCssColor color)
                :: Css.highlightColor vc (Just i == selected)
              )
                |> css
            , UserClicksHighlight i |> onClick
            ]
            [ square ]
        , input
            [ Css.highlightTitle vc |> css
            , type_ "text"
            , value title
            , onInput (UserInputsHighlightTitle i)
            ]
            []
        , span
            [ Css.highlightTrash vc |> css
            , ( UserClickedHighlightTrash i, True )
                |> Json.Decode.succeed
                |> stopPropagationOn "click"
            ]
            [ FontAwesome.icon FontAwesome.trash
                |> Html.Styled.fromUnstyled
            ]
        ]


square : Html Msg
square =
    FontAwesome.icon FontAwesome.square
        |> Html.Styled.fromUnstyled
