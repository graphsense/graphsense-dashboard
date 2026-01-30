module Components.ExportCSV exposing (Config, Model, Msg(..), attributes, config, getNumberOfRows, gotData, icon, init, isDownloading, update)

import Config.Update as Update
import Config.View as View
import Css.Pathfinder exposing (fullWidth)
import Csv.Encode
import File.Download
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events exposing (onClick)
import Model.Notification as Notification exposing (Notification)
import RecordSetter exposing (s_isEphemeral, s_showClose, s_title, s_variables)
import Task
import Theme.Html.Icons as HIcons
import Time
import Util.View exposing (loadingSpinner)
import View.Locale as Locale


type Config data eff
    = Config (ConfigInternal data eff)


type alias ConfigInternal data eff =
    { filename : String
    , toCsv : data -> Maybe (List ( String, String ))
    , numberOfRows : Int
    , fetch : Int -> eff
    , cmdToEff : Cmd Msg -> eff
    , notificationToEff : Notification -> eff
    }


type Model
    = Model ModelInternal


type alias ModelInternal =
    { now : Time.Posix
    , downloading : Bool
    }


type Msg
    = UserClickedExportCSV
    | BrowserGotTime Time.Posix


config :
    { filename : String
    , toCsv : data -> Maybe (List ( String, String ))
    , numberOfRows : Int
    , fetch : Int -> eff
    , cmdToEff : Cmd Msg -> eff
    , notificationToEff : Notification -> eff
    }
    -> Config data eff
config { filename, toCsv, fetch, cmdToEff, notificationToEff, numberOfRows } =
    Config
        { filename = filename
        , toCsv = toCsv
        , fetch = fetch
        , numberOfRows = numberOfRows
        , cmdToEff = cmdToEff
        , notificationToEff = notificationToEff
        }


init : Model
init =
    Model
        { now = Time.millisToPosix 0
        , downloading = False
        }


update : Msg -> Config data eff -> Model -> ( Model, List eff )
update msg (Config conf) (Model model) =
    case msg of
        UserClickedExportCSV ->
            ( Model model
            , Time.now
                |> Task.perform BrowserGotTime
                |> conf.cmdToEff
                |> List.singleton
            )

        BrowserGotTime posix ->
            ( { model
                | downloading = True
                , now = posix
              }
                |> Model
            , [ conf.fetch conf.numberOfRows ]
            )


gotData : Update.Config -> Config data eff -> ( List data, Maybe String ) -> Model -> ( Model, List eff )
gotData uc (Config conf) ( data, nextPage ) (Model model) =
    let
        asCsv =
            Csv.Encode.encode
                { encoder =
                    Csv.Encode.withFieldNames identity
                , fieldSeparator = ','
                }

        filename =
            conf.filename
                |> (++) " "
                |> (++) (Locale.makeTimestampFilename uc.locale model.now)

        notification =
            makeNotification conf.numberOfRows ( data, nextPage )
                |> conf.notificationToEff
    in
    ( Model { model | downloading = False }
    , [ data
            |> List.filterMap conf.toCsv
            |> asCsv
            |> File.Download.string (filename ++ ".csv") "text/csv"
            |> conf.cmdToEff
      , notification
      ]
    )


makeNotification : Int -> ( List data, Maybe String ) -> Notification
makeNotification numberOfRows ( data, nextPage ) =
    nextPage
        |> Maybe.map
            (\_ ->
                "info-more-rows-for-csv-download-hint"
                    |> Notification.infoDefault
                    |> Notification.map
                        (s_isEphemeral False
                            >> s_title (Just "info-more-rows-for-csv-download")
                            >> s_showClose True
                            >> s_variables
                                [ numberOfRows
                                    |> String.fromInt
                                ]
                        )
            )
        |> Maybe.withDefault
            (Notification.successDefault "check download folder"
                |> Notification.map
                    (s_isEphemeral True
                        >> s_title (Just "success_download_csv")
                        >> s_showClose True
                        >> s_variables
                            [ List.length data
                                |> String.fromInt
                            ]
                    )
            )


attributes : Model -> List (Attribute Msg)
attributes (Model { downloading }) =
    if downloading then
        []

    else
        [ onClick UserClickedExportCSV
        , Util.View.pointer
        ]


icon : View.Config -> Model -> Html msg
icon vc (Model { downloading }) =
    if downloading then
        loadingSpinner vc (\_ -> fullWidth)

    else
        HIcons.iconsExport {}


getNumberOfRows : Config data eff -> Int
getNumberOfRows (Config { numberOfRows }) =
    numberOfRows


isDownloading : Model -> Bool
isDownloading (Model { downloading }) =
    downloading
