module Model.Pathfinder.SearchBox exposing
    ( Highlight(..)
    , Model
    , currentMatch
    , empty
    , highlightFor
    , highlightForAny
    , inputId
    , isActive
    , isCurrentMatch
    , isMatch
    )

import List.Extra
import Model.Pathfinder.Id exposing (Id)
import Set exposing (Set)


type alias Model =
    { visible : Bool
    , query : String
    , matches : List Id
    , matchSet : Set Id
    , currentMatchIndex : Maybe Int
    }


type Highlight
    = NoHighlight
    | Dimmed
    | CurrentMatch


inputId : String
inputId =
    "pathfinder-on-graph-search-input"


empty : Model
empty =
    { visible = False
    , query = ""
    , matches = []
    , matchSet = Set.empty
    , currentMatchIndex = Nothing
    }


isActive : Model -> Bool
isActive m =
    m.visible && not (String.isEmpty (String.trim m.query))


isMatch : Model -> Id -> Bool
isMatch m id =
    Set.member id m.matchSet


isCurrentMatch : Model -> Id -> Bool
isCurrentMatch m id =
    currentMatch m == Just id


currentMatch : Model -> Maybe Id
currentMatch m =
    m.currentMatchIndex
        |> Maybe.andThen (\i -> List.Extra.getAt i m.matches)


highlightFor : Model -> Id -> Highlight
highlightFor m id =
    if not (isActive m) then
        NoHighlight

    else if isCurrentMatch m id then
        CurrentMatch

    else if Set.member id m.matchSet then
        NoHighlight

    else
        Dimmed


highlightForAny : Model -> List Id -> Highlight
highlightForAny m ids =
    if not (isActive m) then
        NoHighlight

    else if List.any (\id -> Set.member id m.matchSet) ids then
        NoHighlight

    else
        Dimmed
