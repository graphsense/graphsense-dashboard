module Update exposing (update)

import Browser
import Effect exposing (Effect(..), n)
import Model exposing (Model)
import Msg exposing (..)
import Url exposing (Url)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        UserRequestsUrl request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Url.toString url
                        |> NavPushUrlEffect model.key
                    )

                Browser.External url ->
                    ( model
                    , NavLoadEffect url
                    )

        BrowserChangedUrl url ->
            updateByUrl url model


updateByUrl : Url -> Model -> ( Model, Effect )
updateByUrl _ model =
    n model
