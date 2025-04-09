module Generate exposing (main)

{-| -}

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
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
import RecordSetter as Rs
import Result.Extra
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import String.Format
import Task
import Tuple exposing (first, mapFirst, mapSecond, pair)
import Types exposing (ColorMap)


type alias Flags =
    { api_key : String
    , file_id : String
    , plugin_name : Maybe String
    }


type alias FigmaContent =
    ( Maybe String, List FrameNodeWithChildrenSeparated )


type alias Whitelist =
    { frames : List String
    , components : List String
    }


type Msg
    = GotFigmaMain Flags (Result Http.Error Api.Raw.CanvasNode)
    | GotFrameNodes (Maybe String) (Result Http.Error String)
    | NextIteration


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


type alias Model =
    { files : List Elm.File
    , frames : List ( FrameNodeWithChildrenSeparated, List Elm.Declaration, List Elm.Declaration )
    , whitelist : Whitelist
    , colorMapDark : ColorMapRaw
    , colorMapLight : ColorMapRaw
    , plugin_name : Maybe String
    }


init : Model
init =
    { files = []
    , frames = []
    , whitelist =
        { frames = []
        , components = []
        }
    , colorMapDark = []
    , colorMapLight = []
    , plugin_name = Nothing
    }


main : Program Json.Decode.Value Model Msg
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
                        ( { init
                            | plugin_name = flags.plugin_name
                          }
                        , get flags
                            { url = "/nodes?ids=0:1&depth=1"
                            , expect =
                                Json.Decode.at [ "nodes", "0:1", "document" ] Api.Raw.canvasNodeDecoder
                                    |> Http.expectJson (GotFigmaMain flags)
                            }
                        )

                    Err err1 ->
                        -- decoding plugin figma
                        case Json.Decode.decodeValue decodeFlagsWithColorMaps input of
                            Ok ( colormaps, ( plugin_name, frames ) ) ->
                                let
                                    colorMapLight =
                                        frames
                                            |> findColorMap colorsFrameLight
                                            |> Result.map ((++) colormaps.light)

                                    colorMapDark =
                                        frames
                                            |> findColorMap colorsFrameDark
                                            |> Result.map ((++) colormaps.dark)

                                    model =
                                        { init
                                            | frames =
                                                List.map
                                                    (\frame -> ( frame, [], [] ))
                                                    frames
                                            , plugin_name = plugin_name
                                        }
                                in
                                Result.map2
                                    (\dark light ->
                                        frameNodeToFiles
                                            { model
                                                | colorMapDark = dark
                                                , colorMapLight = light
                                            }
                                    )
                                    colorMapDark
                                    colorMapLight
                                    |> Result.Extra.unpack
                                        (\err ->
                                            ( model
                                            , Generate.error
                                                [ { title = "Error decoding colormap"
                                                  , description = Json.Decode.errorToString err
                                                  }
                                                ]
                                            )
                                        )
                                        identity

                            Err err2 ->
                                -- decoding core figma
                                case Json.Decode.decodeValue decodeWithWhitelist input of
                                    Ok ( whitelist, ( plugin_name, frames ) ) ->
                                        let
                                            colorMapLight =
                                                frames
                                                    |> findColorMap colorsFrameLight

                                            colorMapDark =
                                                frames
                                                    |> findColorMap colorsFrameDark

                                            colorMapFile =
                                                Result.map2
                                                    (\dark light ->
                                                        ( { path = "colormaps.json"
                                                          , warnings = []
                                                          , contents =
                                                                Encode.object
                                                                    [ ( "light", Colors.colorMapToJson light )
                                                                    , ( "dark", Colors.colorMapToJson dark )
                                                                    ]
                                                                    |> Encode.encode 0
                                                          }
                                                        , dark
                                                        , light
                                                        )
                                                    )
                                                    colorMapDark
                                                    colorMapLight
                                        in
                                        colorMapFile
                                            |> Result.Extra.unpack
                                                (\err ->
                                                    ( init
                                                    , Generate.error
                                                        [ { title = "Error decoding colormap"
                                                          , description = Json.Decode.errorToString err
                                                          }
                                                        ]
                                                    )
                                                )
                                                (\( cmf, dark, light ) ->
                                                    let
                                                        _ =
                                                            log "found frames" (List.length frames)

                                                        model =
                                                            { init
                                                                | frames =
                                                                    List.map
                                                                        (\frame -> ( frame, [], [] ))
                                                                        frames
                                                                , colorMapLight = light
                                                                , colorMapDark = dark
                                                                , whitelist = whitelist
                                                                , plugin_name = plugin_name
                                                                , files =
                                                                    [ cmf
                                                                    , Colors.colorMapToStylesheet light
                                                                        :: Colors.colorMapToDeclarations light
                                                                        |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrame ]
                                                                    , Colors.colorMapToStylesheet dark
                                                                        :: Colors.colorMapToDeclarations dark
                                                                        |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrameDark ]
                                                                    ]
                                                            }
                                                    in
                                                    frameNodeToFiles model
                                                )

                                    Err err3 ->
                                        ( init
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
            \msg model ->
                case log "msg" msg of
                    GotFigmaMain flags result ->
                        case result of
                            Ok canvas ->
                                ( model
                                , canvasNodeToRequests flags canvas
                                )

                            Err err ->
                                ( model
                                , Generate.error
                                    [ { title = "Error decoding figma main"
                                      , description = ""
                                      }
                                    ]
                                )

                    GotFrameNodes plugin_name result ->
                        case result of
                            Err err ->
                                ( model
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
                                ( model
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

                    NextIteration ->
                        frameNodeToFiles model
        , subscriptions = \_ -> Sub.none
        }


log : String -> a -> a
log arg1 arg2 =
    --Debug.log ("DEBUG " ++ arg1) arg2
    arg2


decodeFigmaNodesFile : Json.Decode.Decoder (List FrameNodeWithChildrenSeparated)
decodeFigmaNodesFile =
    Json.Decode.dict (Json.Decode.field "document" frameNodeDecoderWithChildrenSeparated)
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


frameNodeToFiles : Model -> ( Model, Cmd Msg )
frameNodeToFiles model =
    case model.frames of
        frame :: _ ->
            frameToFiles model frame

        [] ->
            ( model
            , Generate.files model.files
            )


findColorMap : String -> List FrameNodeWithChildrenSeparated -> Result Json.Decode.Error (List ( RGBA, String ))
findColorMap name =
    List.filter (first >> .frameTraits >> .isLayerTrait >> .name >> (==) name)
        >> List.head
        >> Maybe.map decodeAllChildren
        >> Maybe.map (Result.map Colors.frameNodeToColorMap)
        >> Maybe.withDefault (Ok [])


decodeAllChildren : FrameNodeWithChildrenSeparated -> Result Json.Decode.Error FrameNode
decodeAllChildren ( node, children ) =
    children
        |> List.foldl
            (\child result ->
                result
                    |> Result.map2
                        (\now prev ->
                            prev ++ [ now ]
                        )
                        (Json.Decode.decodeValue Api.Raw.subcanvasNodeDecoder child)
            )
            (Ok [])
        |> Result.map
            (flip Rs.s_children node.frameTraits
                >> flip Rs.s_frameTraits node
            )


colorsFrame : String
colorsFrame =
    "Colors"


colorsFrameLight : String
colorsFrameLight =
    colorsFrame ++ " Light"


colorsFrameDark : String
colorsFrameDark =
    colorsFrame ++ " Dark"


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


frameToFiles : Model -> ( ( FrameNode, List Encode.Value ), List Elm.Declaration, List Elm.Declaration ) -> ( Model, Cmd Msg )
frameToFiles model ( ( n, children ), htmlDeclarations, svgDeclarations ) =
    let
        colorMap =
            List.map (mapFirst (RGBA.toStylesString Dict.empty)) model.colorMapLight
                |> Dict.fromList

        name sub =
            n.frameTraits.isLayerTrait.name
                |> toCamelCaseUpper
                |> List.singleton
                |> (::) sub
                |> (model.plugin_name
                        |> Maybe.map String.Extra.toSentenceCase
                        |> Maybe.map (::)
                        |> Maybe.withDefault identity
                   )
                |> (::) themeFolder

        nameLowered =
            String.toLower n.frameTraits.isLayerTrait.name

        matchOnlyFrames =
            List.isEmpty model.whitelist.frames
                || List.any ((==) nameLowered) (List.map String.toLower model.whitelist.frames)

        restFrames =
            List.drop 1 model.frames

        nextIteration =
            Task.succeed ()
                |> Task.perform (\_ -> NextIteration)

        _ =
            log "decoding frame" nameLowered
    in
    if matchOnlyFrames && not (String.startsWith (String.toLower colorsFrame) nameLowered) then
        case children of
            child :: rest ->
                let
                    _ =
                        log "decoding child" ()
                in
                Json.Decode.decodeValue Api.Raw.subcanvasNodeDecoder child
                    |> Result.Extra.unpack
                        (\err ->
                            ( model
                            , Generate.error
                                [ { title = "Decoding error"
                                  , description = Json.Decode.errorToString err
                                  }
                                ]
                            )
                        )
                        (\ok ->
                            let
                                fun =
                                    Common.subcanvasNodeComponentsToDeclarations

                                updatedFrame =
                                    ( ( n
                                      , rest
                                      )
                                    , htmlDeclarations
                                        ++ fun (Generate.Html.componentNodeToDeclarations colorMap) ok
                                    , svgDeclarations
                                        ++ fun (Generate.Svg.componentNodeToDeclarations colorMap) ok
                                    )

                                _ =
                                    log "rest children length" (List.length rest)
                            in
                            ( { model
                                | frames = updatedFrame :: restFrames
                              }
                            , nextIteration
                            )
                        )

            [] ->
                -- all children decoded
                ( { model
                    | frames = restFrames
                    , files =
                        model.files
                            ++ [ htmlDeclarations
                                    |> Elm.file (name "Svg")
                               , svgDeclarations
                                    |> Elm.file (name "Html")
                               ]
                  }
                , nextIteration
                )

    else
        ( { model | frames = restFrames }
        , nextIteration
        )


frameNodeToDeclarations : (SubcanvasNode -> List Elm.Declaration) -> FrameNode -> List Elm.Declaration
frameNodeToDeclarations gen node =
    List.map gen node.frameTraits.children
        |> List.concat
