module Util.ThemedSelectBoxes exposing (Model, SelectBoxesAvailable(..), get, init, update)

import Dict exposing (Dict)
import Tuple
import Util.ThemedSelectBox as TSb


type SelectBoxesAvailable
    = SupportedLanguages


type Model
    = SBs ModelInternal


toId : SelectBoxesAvailable -> Int
toId sb =
    case sb of
        SupportedLanguages ->
            1


type alias ModelInternal =
    { selectStates : Dict Int TSb.Model
    }


init : List ( SelectBoxesAvailable, TSb.Model ) -> Model
init lst =
    SBs
        { selectStates = Dict.fromList (lst |> List.map (Tuple.mapFirst toId))
        }


update : SelectBoxesAvailable -> TSb.Msg -> Model -> Model
update sb msg m =
    get sb m
        |> Maybe.map (TSb.update msg)
        |> Maybe.map (set sb m)
        |> Maybe.withDefault m


get : SelectBoxesAvailable -> Model -> Maybe TSb.Model
get sb (SBs model) =
    Dict.get (toId sb) model.selectStates


set : SelectBoxesAvailable -> Model -> TSb.Model -> Model
set sb (SBs model) data =
    SBs
        { model
            | selectStates = Dict.insert (toId sb) data model.selectStates
        }
