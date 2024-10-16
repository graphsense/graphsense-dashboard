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
import String.Format
import Tuple exposing (mapFirst, pair)
import Types exposing (ColorMap)


onlyFrames : List String
onlyFrames =
    --[ "side panel components" ]
    []


type alias Flags =
    ( String, String )


type Msg
    = GotFigmaMain Flags (Result Http.Error Api.Raw.CanvasNode)
    | GotFrameNodes (Result Http.Error String)


get : Flags -> { url : String, expect : Http.Expect msg } -> Cmd msg
get ( file_id, api_key ) { url, expect } =
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
        decodeFlags =
            Json.Decode.map2 pair
                (Json.Decode.field "figma_file_id" Json.Decode.string)
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
                        case Json.Decode.decodeValue decodeFigmaNodesFile input of
                            Ok nodes ->
                                ( ()
                                , frameNodesToFiles nodes
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

                    GotFrameNodes result ->
                        case result of
                            Err err ->
                                ( ()
                                , Generate.error
                                    [ { title = "Error fetching figma frames"
                                      , description = Debug.toString err
                                      }
                                    ]
                                )

                            Ok text ->
                                case Json.Decode.decodeString decodeFigmaNodesFile text of
                                    Ok nodes ->
                                        ( ()
                                        , { path = "figma.json"
                                          , contents = text
                                          , warnings = []
                                          }
                                            :: frameNodesToFiles nodes
                                            |> Generate.files
                                        )

                                    Err err ->
                                        ( ()
                                        , Generate.error
                                            [ { title = "Error decoding figma frames"
                                              , description = Debug.toString err
                                              }
                                            ]
                                        )
        , subscriptions = \_ -> Sub.none
        }


decodeFigmaNodesFile : Json.Decode.Decoder (List FrameNode)
decodeFigmaNodesFile =
    Json.Decode.dict (Json.Decode.field "document" Api.Raw.frameNodeDecoder)
        |> Json.Decode.map Dict.values
        |> Json.Decode.field "nodes"


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
        , expect = Http.expectString GotFrameNodes
        }


themeFolder : String
themeFolder =
    "Theme"


frameNodesToFiles : List FrameNode -> List Generate.File
frameNodesToFiles frames =
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
    in
    (Colors.colorMapToStylesheet colorMapLight
        :: Colors.colorMapToDeclarations colorMapLight
        |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrame ]
    )
        :: (Colors.colorMapToStylesheet colorMapDark
                :: Colors.colorMapToDeclarations colorMapDark
                |> Elm.file [ themeFolder, toCamelCaseUpper colorsFrameDark ]
           )
        :: (List.map (frameToFiles colorMapLightDict) frames
                |> List.concat
           )


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
