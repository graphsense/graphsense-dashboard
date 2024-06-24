module Generate exposing (main)

{-| -}

import Api.Raw exposing (..)
import Elm
import Gen.CodeGen.Generate as Generate
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
        |> List.filterMap frameToFile


formatExpression : ( String, Elm.Expression ) -> Elm.Declaration
formatExpression =
    mapFirst toCamelCaseLower
        >> (\( name, expr ) -> Elm.declaration name expr)


frameToFile : SubcanvasNode -> Maybe Generate.File
frameToFile node =
    case node of
        SubcanvasNodeFrameNode n ->
            let
                name =
                    n.frameTraits.isLayerTrait.name
                        |> toCamelCaseUpper
                        |> List.singleton
                        |> (::) "Theme"
            in
            frameNodeToDeclarations n
                |> Elm.file name
                |> Just

        _ ->
            Nothing


frameNodeToDeclarations : FrameNode -> List Elm.Declaration
frameNodeToDeclarations node =
    node.frameTraits.children
        |> List.map
            (if node.frameTraits.isLayerTrait.name == "Pathfinder components" then
                Generate.Svg.subcanvasNodeComponentsToDeclarations

             else
                Debug.todo "Generate.Html.subcanvasNodeToDeclarations"
            )
        |> List.concat
