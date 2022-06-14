module Model.Statusbar exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph.Id as Id
import Model.Graph.Search as Search


type alias Model =
    { messages : Dict String (List String)
    }
