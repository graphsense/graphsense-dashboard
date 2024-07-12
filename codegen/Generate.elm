module Generate exposing (main)

{-| -}

import Api.Raw exposing (..)
import Elm
import Gen.CodeGen.Generate as Generate
import Generate.Html
import Generate.Svg
import Json.Decode
import String.Case exposing (toCamelCaseLower, toCamelCaseUpper)
import Tuple exposing (mapFirst)


main : Program Json.Decode.Value () ()
main =
    Generate.fromJson
        (Json.Decode.field "document" Api.Raw.documentNodeDecoder)
        generate


generate : Api.Raw.DocumentNode -> List Generate.File
generate { children } =
    children
        |> List.map canvasNodeToFiles
        |> List.concat


canvasNodeToFiles : CanvasNode -> List Generate.File
canvasNodeToFiles node =
    node.children
        |> List.map frameToFiles
        |> List.concat


formatExpression : ( String, Elm.Expression ) -> Elm.Declaration
formatExpression =
    mapFirst toCamelCaseLower
        >> (\( name, expr ) -> Elm.declaration name expr)


frameToFiles : SubcanvasNode -> List Generate.File
frameToFiles node =
    case node of
        SubcanvasNodeFrameNode n ->
            let
                name sub =
                    n.frameTraits.isLayerTrait.name
                        |> toCamelCaseUpper
                        |> List.singleton
                        |> (::) sub
                        |> (::) "Theme"
            in
            [ frameNodeToDeclarations
                (Generate.Svg.subcanvasNodeComponentsToDeclarations "")
                n
                |> Elm.file (name "Svg")
            , frameNodeToDeclarations
                (Generate.Html.subcanvasNodeComponentsToDeclarations "")
                n
                |> Elm.file (name "Html")
            ]

        _ ->
            []


frameNodeToDeclarations : (SubcanvasNode -> List Elm.Declaration) -> FrameNode -> List Elm.Declaration
frameNodeToDeclarations gen node =
    if True || node.frameTraits.readyForDev then
        List.map gen node.frameTraits.children
            |> List.concat

    else
        []
