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
import Types exposing (ColorMap)


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


themeFolder : String
themeFolder =
    "Theme"


canvasNodeToFiles : CanvasNode -> List Generate.File
canvasNodeToFiles node =
    let
        frames =
            node.children
                |> List.filterMap isFrame

        colorMap =
            frames
                |> findColorMap
    in
    (Colors.colorMapToStylesheet colorMap
        :: Colors.colorMapToDeclarations colorMap
        |> Elm.file [ themeFolder, "Colors" ]
    )
        :: (List.map (frameToFiles (Dict.fromList colorMap)) frames
                |> List.concat
           )


findColorMap : List FrameNode -> List ( String, String )
findColorMap =
    List.filter (.frameTraits >> .isLayerTrait >> .name >> (==) colorsFrame)
        >> List.head
        >> Maybe.map Colors.frameNodeToColorMap
        >> Maybe.withDefault []


colorsFrame : String
colorsFrame =
    "Colors"


isFrame : SubcanvasNode -> Maybe FrameNode
isFrame arg1 =
    case arg1 of
        SubcanvasNodeFrameNode n ->
            Just n

        _ ->
            Nothing


frameToFiles : ColorMap -> FrameNode -> List Generate.File
frameToFiles colorMap n =
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
    if n.frameTraits.readyForDev && matchOnlyFrames && nameLowered /= String.toLower colorsFrame then
        [ frameNodeToDeclarations
            (Common.subcanvasNodeComponentsToDeclarations (Generate.Svg.componentNodeToDeclarations colorMap))
            n
            |> Elm.file (name "Svg")
        , frameNodeToDeclarations
            (Common.subcanvasNodeComponentsToDeclarations (Generate.Html.componentNodeToDeclarations colorMap))
            n
            |> Elm.file (name "Html")
        ]

    else
        []


frameNodeToDeclarations : (SubcanvasNode -> List Elm.Declaration) -> FrameNode -> List Elm.Declaration
frameNodeToDeclarations gen node =
    List.map gen node.frameTraits.children
        |> List.concat
