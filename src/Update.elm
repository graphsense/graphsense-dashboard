module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Effect exposing (Effect(..), n)
import Model exposing (Model)
import Msg exposing (..)
import Url exposing (Url)


update : Msg -> Model key -> ( Model key, Effect )
update msg model =
    case msg of
        UserRequestsUrl request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Url.toString url
                        |> NavPushUrlEffect
                    )

                Browser.External url ->
                    ( model
                    , NavLoadEffect url
                    )

        BrowserChangedUrl url ->
            updateByUrl url model

        BrowserGotStatistics result ->
            case result of
                Ok stats ->
                    n { model | stats = stats }

                Err _ ->
                    n model


updateByUrl : Url -> Model key -> ( Model key, Effect )
updateByUrl _ model =
    n model
