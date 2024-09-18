module Model.Notification exposing (..)


type alias Model =
    { messages : List Notification
    }


getNext : Model -> Maybe Notification
getNext m =
    List.head m.messages


removeLastNotification : Model -> Model
removeLastNotification m =
    { m | messages = List.tail m.messages |> Maybe.withDefault [] }


type Notification
    = Error String String
    | Info String String
