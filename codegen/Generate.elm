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
import Generate.Util.RGBA as RGBA
import Http
import Json.Decode
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import String.Format
import Tuple exposing (mapFirst, pair)
import Types exposing (ColorMap)


onlyFrames : List String
onlyFrames =
    []
        |> List.map String.toLower


type alias Flags =
    { api_key : String
    , file_id : String
    , plugin_name : Maybe String
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
                |> Debug.log "fetching"
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


main : Program Json.Decode.Value () Msg
main =
    let
        decodeFileId str =
            case String.split ":" str of
                name :: file_id :: [] ->
                    Json.Decode.succeed ( Just name, file_id )

                file_id :: [] ->
                    Json.Decode.succeed ( Nothing, file_id )

                _ ->
                    Json.Decode.fail ("invalid tuple: " ++ str)

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

                    Err _ ->
                        case Json.Decode.decodeValue decodeFigmaNodesFileWithModuleName input of
                            Ok ( plugin_name, nodes ) ->
                                ( ()
                                , frameNodesToFiles plugin_name nodes
                                    |> Generate.files
                                )

                            Err err ->
                                ( ()
                                , Generate.error
                                    [ { title = "Error decoding flags"
                                      , description = Json.Decode.errorToString err
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
                                      , description = Debug.toString err
                                      }
                                    ]
                                )

                    GotFrameNodes plugin_name result ->
                        case result of
                            Err err ->
                                ( ()
                                , Generate.error
                                    [ { title = "Error fetching figma frames"
                                      , description = Debug.toString err
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


decodeFigmaNodesFileWithModuleName : Json.Decode.Decoder ( Maybe String, List FrameNode )
decodeFigmaNodesFileWithModuleName =
    Json.Decode.map2 pair
        (Json.Decode.field "plugin_name" Json.Decode.string |> Json.Decode.maybe)
        (Json.Decode.field "figma" decodeFigmaNodesFile)


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


frameNodesToFiles : Maybe String -> List FrameNode -> List Generate.File
frameNodesToFiles plugin_name frames =
    let
        colorsFrameLight =
            colorsFrame ++ " Light"

        colorsFrameDark =
            colorsFrame ++ " Dark"

        colorMapLight =
            frames
                |> findColorMap colorsFrameLight

        colorMapDark =
            frames
                |> findColorMap colorsFrameDark

        colorMapLightDict =
            List.map (mapFirst (RGBA.toStylesString Dict.empty)) colorMapLight
                |> Dict.fromList

        colorGen =
            if plugin_name == Nothing then
                [ Colors.colorMapToStylesheet colorMapLight
                    :: Colors.colorMapToDeclarations colorMapLight
                    |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrame ]
                , Colors.colorMapToStylesheet colorMapDark
                    :: Colors.colorMapToDeclarations colorMapDark
                    |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrameDark ]
                ]

            else
                []
    in
    (List.map (frameToFiles plugin_name colorMapLightDict) frames
        |> List.concat
    )
        ++ colorGen


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


frameToFiles : Maybe String -> ColorMap -> FrameNode -> List Generate.File
frameToFiles plugin_name colorMap n =
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
                |> (::) "Theme"

        nameLowered =
            String.toLower n.frameTraits.isLayerTrait.name

        matchOnlyFrames =
            List.isEmpty onlyFrames
                || List.any (flip String.startsWith nameLowered) onlyFrames
    in
    if matchOnlyFrames && not (String.startsWith (String.toLower colorsFrame) nameLowered) then
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
