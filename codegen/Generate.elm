module Generate exposing (main)

{-| -}

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict
import Elm
import Gen.CodeGen.Generate as Generate
import Generate.Colors as Colors
import Generate.Common as Common
import Generate.Html
import Generate.Svg
import Json.Decode
import String.Case exposing (toCamelCaseUpper)


onlyFrames : List String
onlyFrames =
    --[ "side panel components" ]
    []


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

                    nameLowered =
                        String.toLower n.frameTraits.isLayerTrait.name

                    matchOnlyFrames =
                        List.isEmpty onlyFrames
                            || List.any (flip String.startsWith nameLowered) onlyFrames
                in
                if n.frameTraits.readyForDev && matchOnlyFrames then
                    [ frameNodeToDeclarations
                        (Common.subcanvasNodeComponentsToDeclarations Generate.Svg.componentNodeToDeclarations)
                        n
                        |> Elm.file (name "Svg")
                    , frameNodeToDeclarations
                        (Common.subcanvasNodeComponentsToDeclarations Generate.Html.componentNodeToDeclarations)
                        n
                        |> Elm.file (name "Html")
                    ]

                else
                    []

        _ ->
            []


frameNodeToDeclarations : (SubcanvasNode -> List Elm.Declaration) -> FrameNode -> List Elm.Declaration
frameNodeToDeclarations gen node =
    List.map gen node.frameTraits.children
        |> List.concat
