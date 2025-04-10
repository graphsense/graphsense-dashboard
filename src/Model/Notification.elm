module Model.Notification exposing (Effect, Model, Msg, Notification(..), NotificationData, add, addMany, empty, fromHttpError, fromHttpErrorWithMoreInfo, getMoved, peek, perform, pop, setMoved, update)

import Basics.Extra exposing (flip)
import Http
import Process
import Set exposing (Set)
import Task
import Tuple exposing (mapSecond, pair)


type alias NotificationData =
    { title : String
    , message : String
    , moreInfo : List String
    , variables : List String
    }


empty : Model
empty =
    NotificationModel { messages = [], messageIds = Set.empty, moved = False }


type Notification
    = Error NotificationData
    | Info NotificationData
    | InfoEphemeral String
    | Success String


type Model
    = NotificationModel InternalModel


type Effect
    = MoveNotification
    | RemoveNotification


type Msg
    = MoveDelayPassed
    | RemoveDelayPassed


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

        effects =
            MoveNotification
                :: (case n of
                        Success _ ->
                            [ RemoveNotification
                            ]

                        InfoEphemeral _ ->
                            [ RemoveNotification ]

                        _ ->
                            []
                   )
    in
    (if Set.member id m.messageIds then
        m

     else
        { m | messages = n :: m.messages, messageIds = Set.insert id m.messageIds }
    )
        |> NotificationModel
        |> flip pair effects


addMany : Model -> List Notification -> ( Model, List Effect )
addMany m ln =
    List.foldl (\n_ ( m_, eff ) -> add n_ m_ |> mapSecond ((++) eff)) ( m, [] ) ln


toId : Notification -> String
toId n =
    case n of
        Error { title, message, variables } ->
            String.join "|" ("error" :: title :: message :: variables)

        Info { title, message, variables } ->
            String.join "|" ("info" :: title :: message :: variables)

        Success title ->
            String.join "|" [ "success", title ]

        InfoEphemeral title ->
            String.join "|" [ "infoEphemeral", title ]


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

        RemoveDelayPassed ->
            unsetMoved model


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        MoveNotification ->
            Process.sleep 0
                |> Task.perform (\_ -> MoveDelayPassed)

        RemoveNotification ->
            Process.sleep 3000
                |> Task.perform (\_ -> RemoveDelayPassed)


fromHttpError : Http.Error -> Notification
fromHttpError =
    fromHttpErrorWithMoreInfo ""


fromHttpErrorWithMoreInfo : String -> Http.Error -> Notification
fromHttpErrorWithMoreInfo info error =
    let
        toMoreInfo =
            (++) [ info ]
                >> List.filter (String.isEmpty >> not)
    in
    case error of
        Http.NetworkError ->
            Error
                { title = "Network error"
                , message = "Service not reachable."
                , moreInfo = toMoreInfo []
                , variables = []
                }

        Http.BadBody body ->
            Error
                { title = "Data error"
                , message = "Unexpected data format."
                , moreInfo = toMoreInfo [ body ]
                , variables = []
                }

        Http.BadUrl _ ->
            Error
                { title = "Bad URL"
                , message = ""
                , moreInfo = toMoreInfo []
                , variables = []
                }

        Http.BadStatus _ ->
            Error
                { title = "Request error"
                , message = "Unexpected status code."
                , moreInfo = toMoreInfo []
                , variables = []
                }

        Http.Timeout ->
            Error
                { title = "Request timeout"
                , message = "Service does not respond in time."
                , moreInfo = toMoreInfo []
                , variables = []
                }
