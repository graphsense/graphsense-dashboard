module Generate exposing (main)

{-| -}

import Api.Raw exposing (..)
import Elm
import Gen.CodeGen.Generate as Generate
import Generate.Colors as Colors
import Generate.Html
import Generate.Svg
import Json.Decode
import String.Case exposing (toCamelCaseUpper)


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


frameToFiles : SubcanvasNode -> List Generate.File
frameToFiles node =
    let
        themeFolder =
            "Theme"
    in
    case node of
        SubcanvasNodeFrameNode n ->
            if n.frameTraits.isLayerTrait.name == "Colors" then
                Colors.frameNodeToDeclarations n
                    |> Elm.file [ themeFolder, "Colors" ]
                    |> List.singleton

            else
                let
                    name sub =
                        n.frameTraits.isLayerTrait.name
                            |> toCamelCaseUpper
                            |> List.singleton
                            |> (::) sub
                            |> (::) themeFolder
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
    if node.frameTraits.readyForDev then
        List.map gen node.frameTraits.children
            |> List.concat

    else
        []
