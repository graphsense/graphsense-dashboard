module Init.Graph.ContextMenu exposing (..)

import Model.Graph.Address as Address
import Model.Graph.ContextMenu exposing (..)
import Model.Graph.Coords exposing (..)
import Model.Graph.Id exposing (..)


initAddress : Coords -> Address.Address -> Model
initAddress coords =
    Address
        >> init coords


init : Coords -> Type -> Model
init =
    Model
