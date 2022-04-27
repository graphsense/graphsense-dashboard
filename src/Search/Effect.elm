module Search.Effect exposing (Effect(..), n, perform)

import Api
import Api.Data
import Api.Request.General
import Bounce
import Http
import Search.Msg exposing (Msg)
import Task
import Time


type Effect
    = NoEffect
    | SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , toMsg : Result Http.Error Api.Data.SearchResult -> Msg
        }
    | BatchEffect (List Effect)
    | CancelEffect
    | BounceEffect Float Msg


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        SearchEffect { query, currency, limit, toMsg } ->
            Api.Request.General.search query currency limit
                |> Api.withTracker "search"
                |> Api.send toMsg

        CancelEffect ->
            Http.cancel "search"

        BatchEffect effs ->
            List.map perform effs
                |> Cmd.batch

        BounceEffect delay msg ->
            Bounce.delay delay msg
