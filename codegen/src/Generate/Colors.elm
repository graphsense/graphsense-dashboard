module Generate.Colors exposing (..)

import Api.Raw exposing (FrameNode, SubcanvasNode(..))
import Basics.Extra exposing (flip, uncurry)
import Dict
import Elm
import Gen.Html.Styled as Html
import Generate.Common.RectangleNode as RectangleNode
import Generate.Util exposing (sanitize)
import Generate.Util.Paint as Paint
import Generate.Util.RGBA as RGBA
import String.Format as Format
import Tuple exposing (pair, second)


frameNodeToColorMap : FrameNode -> List ( String, String )
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
                    |> Maybe.map (RGBA.toStylesString Dict.empty)
                    |> Maybe.map (flip pair (RectangleNode.getName r |> sanitize |> (++) prefix))
            )


prefix : String
prefix =
    "c-"


colorMapToStylesheet : List ( String, String ) -> Elm.Declaration
colorMapToStylesheet =
    List.map (uncurry toCssVar)
        >> String.join "\n"
        >> (\s -> ":root{" ++ s ++ "}")
        >> Html.text
        >> List.singleton
        >> Html.node "style" []
        >> Elm.declaration "style"


toCssVar : String -> String -> String
toCssVar rgba name =
    "--{{ }}: {{ }};"
        |> Format.value name
        |> Format.value rgba


colorMapToDeclarations : List ( String, String ) -> List Elm.Declaration
colorMapToDeclarations =
    List.map (second >> colorToDeclaration)


colorToDeclaration : String -> Elm.Declaration
colorToDeclaration name =
    "var(--"
        ++ name
        ++ ")"
        |> Elm.string
        |> Elm.declaration (String.dropLeft (String.length prefix) name)
