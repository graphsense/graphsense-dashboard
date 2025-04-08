module Generate exposing (main)

{-| -}

import Api.Raw exposing (..)
import Dict
import Elm
import Gen.CodeGen.Generate as Generate
import Generate.Colors as Colors exposing (ColorMapRaw)
import Generate.Common as Common
import Generate.Html
import Generate.Svg
import Generate.Util.RGBA as RGBA
import Http
import Json.Decode
import Json.Encode as Encode
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import String.Format
import Tuple exposing (mapFirst, pair)
import Types exposing (ColorMap)


type alias Flags =
    { api_key : String
    , file_id : String
    , plugin_name : Maybe String
    }


type alias FigmaContent =
    ( Maybe String, List FrameNode )


type alias Whitelist =
    { frames : List String
    , components : List String
    }


type Msg
    = GotFigmaMain Flags (Result Http.Error Api.Raw.CanvasNode)
    | GotFrameNodes (Maybe String) (Result Http.Error String)


get : Flags -> { url : String, expect : Http.Expect msg } -> Cmd msg
get { api_key, file_id } { url, expect } =
    Http.request
        { method = "GET"
        , headers =
            Http.header "X-Figma-Token" api_key
                |> List.singleton
        , url =
            "https://api.figma.com/v1/files/{{ file_id }}"
                ++ url
                |> String.Format.namedValue "file_id" file_id
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


main : Program Json.Decode.Value () Msg
main =
    let
        decodeFlags =
            Json.Decode.map3
                (\plugin_name file_id api_key ->
                    { plugin_name = plugin_name
                    , file_id = file_id
                    , api_key = api_key
                    }
                )
                (Json.Decode.field "plugin_name" Json.Decode.string |> Json.Decode.maybe)
                (Json.Decode.field "figma_file" Json.Decode.string)
                (Json.Decode.field "api_key" Json.Decode.string)

        decodeFlagsWithColorMaps =
            Json.Decode.map2 pair
                (Json.Decode.field "colormaps" Colors.decodeColormaps)
                (Json.Decode.field "theme" decodeFigmaNodesFileWithModuleName)
    in
    Platform.worker
        { init =
            \input ->
                case Json.Decode.decodeValue decodeFlags input of
                    Ok flags ->
                        ( ()
                        , get flags
                            { url = "/nodes?ids=0:1&depth=1"
                            , expect =
                                Json.Decode.at [ "nodes", "0:1", "document" ] Api.Raw.canvasNodeDecoder
                                    |> Http.expectJson (GotFigmaMain flags)
                            }
                        )

                    Err err1 ->
                        case Json.Decode.decodeValue decodeFlagsWithColorMaps input of
                            Ok ( colormaps, ( plugin_name, nodes ) ) ->
                                ( ()
                                , frameNodesToFiles { frames = [], components = [] } colormaps plugin_name nodes
                                    |> Generate.files
                                )

                            Err err2 ->
                                case Json.Decode.decodeValue decodeWithWhitelist input of
                                    Ok ( whitelist, ( plugin_name, nodes ) ) ->
                                        ( ()
                                        , frameNodesToFiles whitelist { light = [], dark = [] } plugin_name nodes
                                            |> Generate.files
                                        )

                                    Err err3 ->
                                        ( ()
                                        , Generate.error
                                            [ { title = "Error decoding flags"
                                              , description = Json.Decode.errorToString err1
                                              }
                                            , { title = "Error decoding flags"
                                              , description = Json.Decode.errorToString err2
                                              }
                                            , { title = "Error decoding flags"
                                              , description = Json.Decode.errorToString err3
                                              }
                                            ]
                                        )
        , update =
            \msg _ ->
                case msg of
                    GotFigmaMain flags result ->
                        case result of
                            Ok canvas ->
                                ( ()
                                , canvasNodeToRequests flags canvas
                                )

                            Err err ->
                                ( ()
                                , Generate.error
                                    [ { title = "Error decoding figma main"
                                      , description = ""
                                      }
                                    ]
                                )

                    GotFrameNodes plugin_name result ->
                        case result of
                            Err err ->
                                ( ()
                                , Generate.error
                                    [ { title = "Error fetching figma frames"
                                      , description = ""
                                      }
                                    ]
                                )

                            Ok json ->
                                let
                                    mn =
                                        plugin_name
                                            |> Maybe.map (\mn_ -> "\"" ++ mn_ ++ "\"")
                                            |> Maybe.withDefault "null"
                                in
                                ( ()
                                , { path = "figma.json"
                                  , warnings = []
                                  , contents =
                                        """{ "plugin_name": {{ plugin_name }}, "figma": {{ json }}}"""
                                            |> String.Format.namedValue "plugin_name" mn
                                            |> String.Format.namedValue "json" json
                                  }
                                    |> List.singleton
                                    |> Generate.files
                                )
        , subscriptions = \_ -> Sub.none
        }


decodeFigmaNodesFile : Json.Decode.Decoder (List FrameNode)
decodeFigmaNodesFile =
    Json.Decode.dict (Json.Decode.field "document" Api.Raw.frameNodeDecoder)
        |> Json.Decode.map Dict.values
        |> Json.Decode.field "nodes"


decodeFigmaNodesFileWithModuleName : Json.Decode.Decoder FigmaContent
decodeFigmaNodesFileWithModuleName =
    Json.Decode.map2 pair
        (Json.Decode.field "plugin_name" Json.Decode.string |> Json.Decode.maybe)
        (Json.Decode.field "figma" decodeFigmaNodesFile)


decodeWithWhitelist : Json.Decode.Decoder ( Whitelist, FigmaContent )
decodeWithWhitelist =
    Json.Decode.map2 pair
        (Json.Decode.map2 Whitelist
            (Json.Decode.field "frames" (Json.Decode.list Json.Decode.string))
            (Json.Decode.field "components" (Json.Decode.list Json.Decode.string))
            |> Json.Decode.field "whitelist"
        )
        (Json.Decode.field "theme" decodeFigmaNodesFileWithModuleName)


canvasNodeToRequests : Flags -> CanvasNode -> Cmd Msg
canvasNodeToRequests flags { children } =
    get flags
        { url =
            "/nodes?ids={{ ids }}&geometry=paths"
                |> String.Format.namedValue "ids"
                    (children
                        |> List.filterMap isFrame
                        |> List.map (.frameTraits >> .isLayerTrait >> .id)
                        |> String.join ","
                    )
        , expect = Http.expectString (GotFrameNodes flags.plugin_name)
        }


themeFolder : String
themeFolder =
    "Theme"


frameNodesToFiles : Whitelist -> { light : ColorMapRaw, dark : ColorMapRaw } -> Maybe String -> List FrameNode -> List Generate.File
frameNodesToFiles whitelist { light, dark } plugin_name frames =
    let
        colorsFrameLight =
            colorsFrame ++ " Light"

        colorsFrameDark =
            colorsFrame ++ " Dark"

        colorMapLight =
            frames
                |> findColorMap colorsFrameLight
                |> (++) light

        colorMapDark =
            frames
                |> findColorMap colorsFrameDark
                |> (++) dark

        colorMapLightDict =
            List.map (mapFirst (RGBA.toStylesString Dict.empty)) colorMapLight
                |> Dict.fromList

        extraFile =
            plugin_name
                |> Maybe.map
                    (\_ -> [])
                |> Maybe.withDefault
                    [ Colors.colorMapToStylesheet colorMapLight
                        :: Colors.colorMapToDeclarations colorMapLight
                        |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrame ]
                    , Colors.colorMapToStylesheet colorMapDark
                        :: Colors.colorMapToDeclarations colorMapDark
                        |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrameDark ]
                    , { path = "colormaps.json"
                      , warnings = []
                      , contents =
                            Encode.object
                                [ ( "light", Colors.colorMapToJson colorMapLight )
                                , ( "dark", Colors.colorMapToJson colorMapDark )
                                ]
                                |> Encode.encode 0
                      }
                    ]
    in
    (List.map (frameToFiles whitelist plugin_name colorMapLightDict) frames
        |> List.concat
    )
        ++ extraFile


findColorMap : String -> List FrameNode -> List ( RGBA, String )
findColorMap name =
    List.filter (.frameTraits >> .isLayerTrait >> .name >> (==) name)
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
            if n.frameTraits.readyForDev then
                Just n

            else
                Nothing

        _ ->
            Nothing


frameToFiles : Whitelist -> Maybe String -> ColorMap -> FrameNode -> List Generate.File
frameToFiles whitelist plugin_name colorMap n =
    let
        name sub =
            n.frameTraits.isLayerTrait.name
                |> toCamelCaseUpper
                |> List.singleton
                |> (::) sub
                |> (plugin_name
                        |> Maybe.map String.Extra.toSentenceCase
                        |> Maybe.map (::)
                        |> Maybe.withDefault identity
                   )
                |> (::) themeFolder

        nameLowered =
            String.toLower n.frameTraits.isLayerTrait.name

        matchOnlyFrames =
            List.isEmpty whitelist.frames
                || List.any ((==) nameLowered) (List.map String.toLower whitelist.frames)
    in
    if matchOnlyFrames && not (String.startsWith (String.toLower colorsFrame) nameLowered) then
        [ frameNodeToDeclarations
            (Common.subcanvasNodeComponentsToDeclarations whitelist.components (Generate.Svg.componentNodeToDeclarations colorMap))
            n
            |> Elm.file (name "Svg")
        , frameNodeToDeclarations
            (Common.subcanvasNodeComponentsToDeclarations whitelist.components (Generate.Html.componentNodeToDeclarations colorMap))
            n
            |> Elm.file (name "Html")
        ]

    else
        []


frameNodeToDeclarations : (SubcanvasNode -> List Elm.Declaration) -> FrameNode -> List Elm.Declaration
frameNodeToDeclarations gen node =
    List.map gen node.frameTraits.children
        |> List.concat
