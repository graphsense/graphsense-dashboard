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
import Json.Decode as Decode
import Json.Encode as Encode
import String.Format as Format
import Tuple exposing (pair)


type alias ColorMapRaw =
    List ( RGBA, String )


frameNodeToColorMap : FrameNode -> ColorMapRaw
frameNodeToColorMap node =
    findColorRects node.frameTraits
        |> List.filterMap
            (\r ->
                Paint.toRGBA r.rectangularShapeTraits.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait.fills
                    --|> Maybe.map (RGBA.toStylesString Dict.empty)
                    |> Maybe.map (flip pair (RectangleNode.getName r |> sanitize |> (++) prefix))
            )


findColorRects : Api.Raw.FrameTraits -> List Api.Raw.RectangleNode
findColorRects frameTraits =
    frameTraits.children
        |> List.map
            (\c ->
                case c of
                    SubcanvasNodeRectangleNode n ->
                        [ n ]

                    SubcanvasNodeFrameNode n ->
                        findColorRects n.frameTraits

                    SubcanvasNodeGroupNode n ->
                        findColorRects n.frameTraits

                    _ ->
                        []
            )
        |> List.concat


prefix : String
prefix =
    "c-"


colorMapToJson : ColorMapRaw -> Encode.Value
colorMapToJson =
    (\( { r, g, b, a }, str ) ->
        Encode.object
            [ ( "r", Encode.float r )
            , ( "g", Encode.float g )
            , ( "b", Encode.float b )
            , ( "a", Encode.float a )
            , ( "name", Encode.string str )
            ]
    )
        |> Encode.list


colorMapFromJson : Decode.Decoder ColorMapRaw
colorMapFromJson =
    Api.Raw.rGBADecoder
        |> Decode.andThen
            (\rgba ->
                Decode.field "name" Decode.string
                    |> Decode.map (pair rgba)
            )
        |> Decode.list


decodeColormaps : Decode.Decoder { light : ColorMapRaw, dark : ColorMapRaw }
decodeColormaps =
    Decode.map2 (\light dark -> { light = light, dark = dark })
        (Decode.field "light" colorMapFromJson)
        (Decode.field "dark" colorMapFromJson)


colorMapToStylesheet : ColorMapRaw -> Elm.Declaration
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


colorMapToDeclarations : ColorMapRaw -> List Elm.Declaration
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
    , Elm.string ("--" ++ name)
        |> Elm.declaration (n ++ "_name")
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
