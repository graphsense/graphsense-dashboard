module Update.Popup exposing (..)

import Model.Popup exposing (..)


update : ( Float, Float ) -> Model -> Model
update ( dx, dy ) model =
    { model
        | x = model.x + dx
        , y = model.y + dy
    }
