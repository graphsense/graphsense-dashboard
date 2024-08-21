module Model.Pathfinder.Colors exposing (ColorScope(..), ScopedColorAssignment, assignNextColor, getAssignedColor, hasAssingedColor, init)

import Color exposing (Color)
import Dict
import Iknaio.ColorScheme exposing (colorScheme)
import List.Extra exposing (getAt)
import Model.Pathfinder.Id as Id exposing (Id)


type ColorScope
    = Clusters


scopeToId : ColorScope -> String
scopeToId s =
    case s of
        Clusters ->
            "clusters"


type alias ReuseableColor =
    { color : Color, timesUsed : Int }


type alias ColorAssingment =
    { currentIndex : Int
    , assignments : Dict.Dict Id ReuseableColor
    , colorSet : List Color
    }


type alias ScopedColorAssignment =
    Dict.Dict String ColorAssingment


getAssignedColor : ColorScope -> Id -> ScopedColorAssignment -> Maybe ReuseableColor
getAssignedColor cs id m =
    Dict.get (scopeToId cs) m |> Maybe.andThen (.assignments >> Dict.get id)


hasAssingedColor : ColorScope -> Id -> ScopedColorAssignment -> Bool
hasAssingedColor cs id m =
    getAssignedColor cs id m |> (/=) Nothing


assignNextColor : ColorScope -> Id -> ScopedColorAssignment -> ScopedColorAssignment
assignNextColor cs id m =
    Dict.update (scopeToId cs) (Maybe.map (assignNextColor_ id)) m


assignNextColor_ : Id -> ColorAssingment -> ColorAssingment
assignNextColor_ id c =
    if Dict.member id c.assignments then
        c

    else
        let
            nClrs =
                List.length c.colorSet

            indx =
                modBy nClrs c.currentIndex

            nextCol =
                getAt indx c.colorSet
        in
        case nextCol of
            Just color ->
                { c | currentIndex = c.currentIndex + 1, assignments = Dict.insert id { color = color, timesUsed = c.currentIndex // nClrs } c.assignments }

            _ ->
                c


init : ScopedColorAssignment
init =
    Dict.fromList [ ( Clusters |> scopeToId, { currentIndex = 0, assignments = Dict.empty, colorSet = colorScheme } ) ]
