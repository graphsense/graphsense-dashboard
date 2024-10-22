module Model.Notification exposing (Effect, Model, Msg, Notification(..), NotificationData, add, addMany, empty, getMoved, peek, perform, pop, setMoved, update)

import Basics.Extra exposing (flip)
import Process
import Set exposing (Set)
import Task
import Tuple exposing (mapSecond, pair)


type alias NotificationData =
    { title : String
    , message : String
    , variables : List String
    }


empty : Model
empty =
    NotificationModel { messages = [], messageIds = Set.empty, moved = False }


type Notification
    = Error NotificationData
    | Info NotificationData


type Model
    = NotificationModel InternalModel


type Effect
    = MoveNotification


type Msg
    = MoveDelayPassed


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
    in
    (if Set.member id m.messageIds then
        m

     else
        { m | messages = n :: m.messages, messageIds = Set.insert id m.messageIds }
    )
        |> NotificationModel
        |> flip pair [ MoveNotification ]


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


getMoved : Model -> Bool
getMoved (NotificationModel { moved }) =
    moved


setMoved : Model -> Model
setMoved (NotificationModel m) =
    { m | moved = True }
        |> NotificationModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveDelayPassed ->
            setMoved model


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        MoveNotification ->
            Process.sleep 0
                |> Task.perform (\_ -> MoveDelayPassed)
