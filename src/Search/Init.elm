module Search.Init exposing (init)

import Bounce
import RemoteData exposing (RemoteData(..))
import Search.Model exposing (Model)


init : Model
init =
    { result = NotAsked
    , found = Nothing
    , input = ""
    , bounce = Bounce.init
    }
