module Init exposing (init)

import Config exposing (config)
import Effect exposing (Effect(..), n)
import Locale.Init as Locale
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


init : Flags -> Url -> key -> ( Model key, Effect )
init _ url key =
    n
        { url = url
        , key = key
        , config = config
        , locale = Locale.init
        , search = ()
        , user = ()
        , stats = NotAsked
        }
        |> getStatistics


getStatistics : ( Model key, Effect ) -> ( Model key, Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = Loading }
        , Effect.batch [ eff, GetStatisticsEffect ]
        )

    else
        ( model, eff )
