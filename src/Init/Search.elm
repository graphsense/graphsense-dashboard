module Init.Search exposing (init)

import Bounce
import Model.Search exposing (Model)
import RemoteData exposing (RemoteData(..))


init : Model
init =
    { loading = False
    , visible = False
    , found = Nothing
    , input = ""
    , bounce = Bounce.init
    , batch = Nothing
    }