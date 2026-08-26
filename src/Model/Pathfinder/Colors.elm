module Model.Pathfinder.Colors exposing (ColorAssingment, ColorScope(..), ReuseableColor, ScopedColorAssignment, assignNextColor, getAssignedColor, init)

import Dict exposing (Dict)
import List.Extra exposing (getAt)
import Model.Pathfinder.Id exposing (Id)
import Theme.Colors as Colors


colorSchemePathfinder : List String
colorSchemePathfinder =
    [ Colors.cluster1
    , Colors.cluster2
    , Colors.cluster3
    , Colors.cluster4
    , Colors.cluster5
    , Colors.cluster6
    , Colors.cluster7
    , Colors.cluster8
    , Colors.cluster9
    , Colors.cluster10
    ]


type ColorScope
    = Clusters


scopeToId : ColorScope -> String
scopeToId s =
    case s of
        Clusters ->
            "clusters"


type alias ReuseableColor =
    { color : String, timesUsed : Int }


type alias ColorAssingment =
    { currentIndex : Int
    , assignments : Dict Id ReuseableColor
    , colorSet : List String
    }


type alias ScopedColorAssignment =
    Dict String ColorAssingment


getAssignedColor : ColorScope -> Id -> ScopedColorAssignment -> Maybe ReuseableColor
getAssignedColor cs id m =
    Dict.get (scopeToId cs) m |> Maybe.andThen (.assignments >> Dict.get id)


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
    Dict.fromList [ ( Clusters |> scopeToId, { currentIndex = 0, assignments = Dict.empty, colorSet = colorSchemePathfinder } ) ]
