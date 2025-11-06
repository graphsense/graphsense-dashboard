module Components.ExportCSV exposing (Config, Model, Msg, attributes, config, gotData, icon, init, makeNotification, update)

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


type Config data
    = Config (ConfigInternal data)


type alias ConfigInternal data =
    { filename : String
    , toCsv : data -> Maybe (List ( String, String ))
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
    }
    -> Config data
config { filename, toCsv } =
    Config
        { filename = filename
        , toCsv = toCsv
        }


init : Model
init =
    Model
        { now = Time.millisToPosix 0
        , downloading = False
        }


update : Update.Config -> Config data -> Msg -> Model -> ( Model, Cmd Msg, Bool )
update uc (Config conf) msg (Model model) =
    case msg of
        UserClickedExportCSV ->
            ( Model model
            , Time.now
                |> Task.perform BrowserGotTime
            , False
            )

        BrowserGotTime posix ->
            ( { model
                | downloading = True
                , now = posix
              }
                |> Model
            , Cmd.none
            , True
            )


gotData : Update.Config -> Config data -> List data -> Model -> ( Model, Cmd Msg, Bool )
gotData uc (Config conf) data (Model model) =
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
    in
    ( Model { model | downloading = False }
    , data
        |> List.filterMap conf.toCsv
        |> asCsv
        |> File.Download.string (filename ++ ".csv") "text/csv"
    , False
    )


makeNotification : Int -> Notification
makeNotification numberOfRows =
    "there_were_more_rows_for_csv_download_info"
        |> Notification.infoDefault
        |> Notification.map
            (s_isEphemeral False
                >> s_title (Just "there_were_more_rows_for_csv_download")
                >> s_showClose True
                >> s_variables
                    [ numberOfRows
                        |> String.fromInt
                    ]
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
