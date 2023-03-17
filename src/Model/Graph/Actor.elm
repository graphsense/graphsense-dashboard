module Model.Graph.Actor exposing (..)

import Api.Data
import List.Extra


type alias Actor =
    { id : String
    , label : String
    , uri : String
    , categories : List String
    , jurisdictions : List String
    , context : Maybe Api.Data.ActorContext
    , nrTags : Maybe Int
    }


getImageUri : Actor -> Maybe String
getImageUri actor =
    actor.context |> Maybe.andThen (\ctx -> List.Extra.getAt 0 ctx.images)


getUris : Actor -> List String
getUris actor =
    [ actor.uri ]
        ++ (actor.context
                |> Maybe.map (\ctx -> ctx.uris)
                |> Maybe.withDefault []
           )
        ++ (actor.context
                |> Maybe.andThen (\ctx -> ctx.twitterHandle |> Maybe.map (\twh -> String.split "," twh))
                |> Maybe.withDefault []
                |> List.map (\x -> "https://twitter.com/" ++ String.trim x)
           )
