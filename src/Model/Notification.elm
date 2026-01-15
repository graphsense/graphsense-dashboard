module Model.Notification exposing (Effect, Model, Msg, Notification(..), NotificationData, add, addMany, empty, errorDefault, fromHttpError, fromHttpErrorWithMoreInfo, getMoved, infoDefault, map, peek, perform, pop, setMoved, successDefault, update)

import Basics.Extra exposing (flip)
import Http
import Process
import RecordSetter as Rs
import Set exposing (Set)
import Task
import Tuple exposing (mapSecond, pair)


type alias NotificationData =
    { title : Maybe String
    , message : String
    , moreInfo : List String
    , variables : List String
    , showClose : Bool
    , isEphemeral : Bool
    , removeDelayMs : Float
    }


type Notification
    = Error NotificationData
    | Info NotificationData
    | Success NotificationData


defaultNotificationData : String -> NotificationData
defaultNotificationData message =
    { title = Nothing, message = message, moreInfo = [], variables = [], showClose = True, isEphemeral = False, removeDelayMs = 12000 }


successDefault : String -> Notification
successDefault message =
    Success (defaultNotificationData message)


errorDefault : String -> Notification
errorDefault message =
    Error (defaultNotificationData message)


infoDefault : String -> Notification
infoDefault message =
    Info (defaultNotificationData message)


getNotificationData : Notification -> NotificationData
getNotificationData n =
    case n of
        Success d ->
            d

        Info d ->
            d

        Error d ->
            d


map : (NotificationData -> NotificationData) -> Notification -> Notification
map fn n =
    case n of
        Success d ->
            fn d |> Success

        Info d ->
            fn d |> Info

        Error d ->
            fn d |> Error


empty : Model
empty =
    NotificationModel { messages = [], messageIds = Set.empty, moved = False }


type Model
    = NotificationModel InternalModel


type Effect
    = MoveNotification
    | RemoveNotification Notification


type Msg
    = MoveDelayPassed
    | RemoveDelayPrePassed Notification
    | RemoveDelayPassed Notification


type alias InternalModel =
    { messages : List Notification
    , messageIds : Set String
    , moved : Bool
    }


peek : Model -> Maybe Notification
peek (NotificationModel m) =
    List.head m.messages


pop : Model -> Model
pop (NotificationModel m) =
    let
        messages =
            List.tail m.messages |> Maybe.withDefault []
    in
    { m
        | messages = messages
        , messageIds = List.head m.messages |> Maybe.map (toId >> flip Set.remove m.messageIds) |> Maybe.withDefault m.messageIds
        , moved = List.isEmpty messages |> not
    }
        |> NotificationModel


add : Notification -> Model -> ( Model, List Effect )
add n (NotificationModel m) =
    let
        id =
            n |> toId

        produceEff d =
            if d.isEphemeral then
                [ RemoveNotification n ]

            else
                []

        effects =
            MoveNotification
                :: (case n of
                        Success d ->
                            produceEff d

                        Info d ->
                            produceEff d

                        Error d ->
                            produceEff d
                   )
    in
    (if Set.member id m.messageIds then
        m

     else
        { m | messages = n :: m.messages, messageIds = Set.insert id m.messageIds }
    )
        |> NotificationModel
        |> flip pair effects


remove : Notification -> Model -> Model
remove n (NotificationModel m) =
    let
        mnew =
            m.messages |> List.filter ((/=) n)
    in
    { m
        | messages = mnew
        , messageIds = mnew |> List.map toId |> Set.fromList
    }
        |> NotificationModel


addMany : Model -> List Notification -> ( Model, List Effect )
addMany m ln =
    List.foldl (\n_ ( m_, eff ) -> add n_ m_ |> mapSecond ((++) eff)) ( m, [] ) ln


toId : Notification -> String
toId n =
    let
        dId { title, message, variables, isEphemeral } =
            (title |> Maybe.withDefault "")
                :: message
                :: (if isEphemeral then
                        "yes"

                    else
                        "no"
                   )
                :: variables
    in
    String.join "|"
        (case n of
            Error data ->
                "error" :: dId data

            Info data ->
                "info" :: dId data

            Success data ->
                "success" :: dId data
        )


getMoved : Model -> Bool
getMoved (NotificationModel { moved }) =
    moved


setMoved : Model -> Model
setMoved (NotificationModel m) =
    { m | moved = True }
        |> NotificationModel


unsetMoved : Model -> Model
unsetMoved (NotificationModel m) =
    { m | moved = False }
        |> NotificationModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveDelayPassed ->
            setMoved model

        RemoveDelayPrePassed n ->
            if peek model == Just n then
                unsetMoved model

            else
                model

        RemoveDelayPassed n ->
            if peek model == Just n then
                unsetMoved model |> remove n

            else
                model |> remove n


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        MoveNotification ->
            Process.sleep 0
                |> Task.perform (MoveDelayPassed |> always)

        RemoveNotification n ->
            Cmd.batch
                [ Process.sleep (n |> getNotificationData |> .removeDelayMs)
                    |> Task.perform (RemoveDelayPassed n |> always)
                , Process.sleep ((n |> getNotificationData |> .removeDelayMs) - 300)
                    |> Task.perform (RemoveDelayPrePassed n |> always)
                ]


fromHttpError : Http.Error -> Notification
fromHttpError =
    fromHttpErrorWithMoreInfo ""


fromHttpErrorWithMoreInfo : String -> Http.Error -> Notification
fromHttpErrorWithMoreInfo infoData errorData =
    let
        toMoreInfo =
            (++) [ infoData ]
                >> List.filter (String.isEmpty >> not)
    in
    case errorData of
        Http.NetworkError ->
            errorDefault "Service not reachable"
                |> map (Rs.s_title (Just "Network Error"))
                |> map (Rs.s_moreInfo (toMoreInfo []))

        Http.BadBody body ->
            errorDefault "Unexpected data format"
                |> map (Rs.s_title (Just "Data Error"))
                |> map (Rs.s_moreInfo (toMoreInfo [ body ]))

        Http.BadUrl _ ->
            errorDefault "Unexpected data format"
                |> map (Rs.s_title (Just "Bad URL"))
                |> map (Rs.s_moreInfo (toMoreInfo []))

        Http.BadStatus _ ->
            errorDefault "Unexpected status code"
                |> map (Rs.s_title (Just "Request error"))
                |> map (Rs.s_moreInfo (toMoreInfo []))

        Http.Timeout ->
            errorDefault "Service does not respond in time"
                |> map (Rs.s_title (Just "Request timeout"))
                |> map (Rs.s_moreInfo (toMoreInfo []))
