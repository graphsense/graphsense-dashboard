module View.Stats exposing (stats)

import Api.Data
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Locale.Model as Locale
import Model exposing (..)
import Msg exposing (..)
import RemoteData as RD exposing (WebData)


stats : Locale.Model -> WebData Api.Data.Stats -> Html Msg
stats locale =
    RD.map .version
        >> RD.withDefault "no_version"
        >> text
