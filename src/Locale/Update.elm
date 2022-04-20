module Locale.Update exposing (..)

import Locale.Effect as Effect exposing (Effect(..), n)
import Locale.Model as Model exposing (..)
import Locale.Msg as Msg exposing (Msg(..))


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        BrowserLoadedTranslation locale result ->
            result
                |> Result.map
                    (\mapping ->
                        { model | mapping = mapping }
                    )
                |> Result.withDefault model
                |> n
