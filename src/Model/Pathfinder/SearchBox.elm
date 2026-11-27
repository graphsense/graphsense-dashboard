module Model.Pathfinder.SearchBox exposing
    ( Highlight(..)
    , Model
    , currentMatch
    , dimmedOpacity
    , empty
    , highlightFor
    , highlightForAny
    , inputId
    )

import Css
import List.Extra
import Model.Pathfinder.Id exposing (Id)
import Set exposing (Set)
import Svg.Styled exposing (Attribute)
import Svg.Styled.Attributes exposing (css, opacity)


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
      -- hidden by the on-graph filters, drawn because "show hidden edges in
      -- grey" is on
    | FilteredOut


inputId : String
inputId =
    "pathfinder-on-graph-search-input"


{-| Append opacity "0.25" to attributes if highlight is Dimmed
-}
dimmedOpacity : Highlight -> List (Attribute msg)
dimmedOpacity highlight =
    case highlight of
        Dimmed ->
            [ opacity "0.25" ]

        FilteredOut ->
            [ opacity "0.35", css [ Css.property "filter" "grayscale(1)" ] ]

        _ ->
            []


empty : Model
empty =
    { visible = False
    , query = ""
    , matches = []
    , matchSet = Set.empty
    , currentMatchIndex = Nothing
    }


{-| Searching, and something on the graph matches. A query that matches
nothing greys out nothing: the box already says nothing was found, and a
graph turned all grey says nothing more.
-}
isActive : Model -> Bool
isActive m =
    m.visible
        && not (String.isEmpty (String.trim m.query))
        && not (Set.isEmpty m.matchSet)


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
