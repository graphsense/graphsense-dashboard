module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Effect exposing (Effect(..), n)
import Locale.Update as Locale
import Model exposing (Model)
import Msg exposing (..)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Search.Update as Search
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
                    n { model | stats = Success stats }

                Err error ->
                    n { model | stats = Failure error }

        LocaleMsg m ->
            let
                ( locale, localeEffect ) =
                    Locale.update m model.locale
            in
            ( { model | locale = locale }
            , LocaleEffect localeEffect
            )

        SearchMsg m ->
            let
                ( search, searchEffect ) =
                    Search.update m model.search
            in
            ( { model | search = search }
            , SearchEffect searchEffect
            )


updateByUrl : Url -> Model key -> ( Model key, Effect )
updateByUrl _ model =
    n model
