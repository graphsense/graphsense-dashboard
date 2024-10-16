module Generate.Colors exposing (..)

import Api.Raw exposing (FrameNode, RGBA, SubcanvasNode(..))
import Basics.Extra exposing (flip, uncurry)
import Dict
import Elm
import Gen.Color as Color
import Gen.Html.Styled as Html
import Generate.Common.RectangleNode as RectangleNode
import Generate.Util exposing (sanitize)
import Generate.Util.Paint as Paint
import Generate.Util.RGBA as RGBA
import String.Format as Format
import Tuple exposing (pair)


frameNodeToColorMap : FrameNode -> List ( RGBA, String )
frameNodeToColorMap node =
    node.frameTraits.children
        |> List.filterMap
            (\c ->
                case c of
                    SubcanvasNodeRectangleNode n ->
                        Just n

                    _ ->
                        Nothing
            )
        |> List.filterMap
            (\r ->
                Paint.toRGBA r.rectangularShapeTraits.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait.fills
                    --|> Maybe.map (RGBA.toStylesString Dict.empty)
                    |> Maybe.map (flip pair (RectangleNode.getName r |> sanitize |> (++) prefix))
            )


prefix : String
prefix =
    "c-"


colorMapToStylesheet : List ( RGBA, String ) -> Elm.Declaration
colorMapToStylesheet =
    List.map (uncurry toCssVar)
        >> String.join "\n"
        >> (\s -> ":root{" ++ s ++ "}")
        >> Html.text
        >> List.singleton
        >> Html.node "style" []
        >> Elm.declaration "style"


toCssVar : RGBA -> String -> String
toCssVar rgba name =
    "--{{ }}: {{ }};"
        |> Format.value name
        |> Format.value (RGBA.toStylesString Dict.empty rgba)


colorMapToDeclarations : List ( RGBA, String ) -> List Elm.Declaration
colorMapToDeclarations =
    List.map (uncurry colorToDeclarations)
        >> List.concat


colorToDeclarations : RGBA -> String -> List Elm.Declaration
colorToDeclarations color name =
    let
        n =
            String.dropLeft (String.length prefix) name
    in
    [ "var(--"
        ++ name
        ++ ")"
        |> Elm.string
        |> Elm.declaration n
    , RGBA.toStylesString Dict.empty color
        |> Elm.string
        |> Elm.declaration (n ++ "_string")
    , Color.fromRgba
        { red = color.r
        , green = color.g
        , blue = color.b
        , alpha = color.a
        }
        |> Elm.declaration (n ++ "_color")
    ]
