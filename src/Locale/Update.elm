module Locale.Update exposing (..)

import Dict
import Locale.Effect as Effect exposing (Effect(..), n)
import Locale.Model as Model exposing (..)
import Locale.Msg as Msg exposing (Msg(..))
import RemoteData


duration : Float
duration =
    700


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        BrowserSentTimezone zone ->
            n { model | zone = zone }

        BrowserLoadedTranslation locale result ->
            result
                |> Result.map
                    (\mapping ->
                        { model
                            | mapping =
                                case model.mapping of
                                    Empty ->
                                        Transition Dict.empty mapping 0

                                    Transition start end delta ->
                                        Transition start mapping delta

                                    Settled end ->
                                        Transition end mapping 0
                        }
                    )
                |> Result.withDefault model
                |> n

        RuntimeTick delta ->
            case model.mapping of
                Transition start end curr ->
                    let
                        now =
                            curr
                                + delta
                                |> Debug.log "now"
                    in
                    { model
                        | mapping =
                            if now > duration then
                                Settled end

                            else
                                Transition start end now
                    }
                        |> n

                _ ->
                    n model
