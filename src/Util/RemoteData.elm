module Util.RemoteData exposing (webdata)

import Html.Styled exposing (..)
import Http
import RemoteData exposing (RemoteData(..), WebData)


type alias WebDataConfig a msg =
    { onFailure : Http.Error -> Html msg
    , onNotAsked : Html msg
    , onLoading : Html msg
    , onSuccess : a -> Html msg
    }


webdata : WebDataConfig a msg -> WebData a -> Html msg
webdata config data =
    case data of
        NotAsked ->
            config.onNotAsked

        Loading ->
            config.onLoading

        Failure err ->
            config.onFailure err

        Success a ->
            config.onSuccess a
