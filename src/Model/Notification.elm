module Model.Notification exposing (Model, Notification(..), NotificationData, add, addMany, empty, peek, pop)

import Basics.Extra exposing (flip)
import Set exposing (Set)


type alias NotificationData =
    { title : String
    , message : String
    , variables : List String
    }


empty : Model
empty =
    NotificationModel { messages = [], messageIds = Set.empty }


type Notification
    = Error NotificationData
    | Info NotificationData


type Model
    = NotificationModel InternalModel


type alias InternalModel =
    { messages : List Notification
    , messageIds : Set String
    }


peek : Model -> Maybe Notification
peek (NotificationModel m) =
    List.head m.messages


pop : Model -> Model
pop (NotificationModel m) =
    { m
        | messages = List.tail m.messages |> Maybe.withDefault []
        , messageIds = List.head m.messages |> Maybe.map (toId >> flip Set.remove m.messageIds) |> Maybe.withDefault m.messageIds
    }
        |> NotificationModel


add : Model -> Notification -> Model
add (NotificationModel m) n =
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


addMany : Model -> List Notification -> Model
addMany m ln =
    List.foldl (flip add) m ln


toId : Notification -> String
toId n =
    case n of
        Error { title, message, variables } ->
            String.join "|" ("error" :: title :: message :: variables)

        Info { title, message, variables } ->
            String.join "|" ("info" :: title :: message :: variables)
