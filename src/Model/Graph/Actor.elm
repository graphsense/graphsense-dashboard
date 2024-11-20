module Model.Graph.Actor exposing (Actor, getImageUri, getUris, getUrisWithoutMain)

import Api.Data
import List.Extra


type alias Actor =
    { id : String
    , label : String
    , uri : String
    , categories : List Api.Data.LabeledItemRef
    , jurisdictions : List Api.Data.LabeledItemRef
    , context : Maybe Api.Data.ActorContext
    , nrTags : Maybe Int
    }


getImageUri : Actor -> Maybe String
getImageUri actor =
    actor.context |> Maybe.andThen (\ctx -> List.Extra.getAt 0 ctx.images)


getUris : Actor -> List String
getUris actor =
    actor.uri
        :: getUrisWithoutMain actor


getUrisWithoutMain : Actor -> List String
getUrisWithoutMain actor =
    (actor.context
        |> Maybe.map (\ctx -> ctx.uris)
        |> Maybe.withDefault []
    )
        ++ (actor.context
                |> Maybe.map (\ctx -> ctx.refs)
                |> Maybe.withDefault []
           )
        ++ (actor.context
                |> Maybe.andThen (\ctx -> ctx.twitterHandle |> Maybe.map (\twh -> String.split "," twh))
                |> Maybe.withDefault []
                |> List.map (\x -> "https://twitter.com/" ++ String.trim x)
           )
        ++ (actor.context
                |> Maybe.andThen (\ctx -> ctx.githubOrganisation |> Maybe.map (\twh -> String.split "," twh))
                |> Maybe.withDefault []
                |> List.map (\x -> "https://github.com/" ++ String.trim x)
           )
