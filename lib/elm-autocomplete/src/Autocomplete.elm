module Autocomplete exposing
    ( Autocomplete, Msg, Choices, ViewState, ViewStatus(..)
    , init, update
    , selectedValue, enoughCharacters
    , setChoices, setQuery, setSelectedIndex, setStatus
    , viewState, query, choices, inFocus, isFocus, isInput, isBlur
    , onFetch
    )

{-| Autocomplete contains the main logic to handle auto-complete.
The state and logic of Autocomplete should reside within your model.
Please refer to our examples to see how it all linked up.

To render the Autocomplete, please refer to `Autocomplete.View` or `Autocomplete.Styled`.


# Types

@docs Autocomplete, Msg, Choices, ViewState, ViewStatus


# State Management

@docs init, update


# Helpers

@docs selectedValue, enoughCharacters


# Setters

@docs setChoices, setQuery, setSelectedIndex, setStatus


# Accessors

@docs viewState, query, choices, inFocus, isFocus, isInput, isBlur

-}

import Bounce
import Internal exposing (Msg(..))


{-| Opaque type of Autocomplete state
-}
type Autocomplete a
    = Autocomplete (State a)


{-| Opaque type of Autocomplete internal msg
-}
type alias Msg =
    Internal.Msg


{-| Record to hold the query and choices for your fetcher function.

    type alias Choices a =
        { query : String
        , choices : List a
        , ignoreList : List a
        }

    fetcher : Autocomplete.Choices String -> Task String (Autocomplete.Choices String)
    fetcher lastChoices =
        let
            dogs =
                [ "Hunter"
                , "Polo"
                , "Loki"
                , "Angel"
                , "Scout"
                , "Lexi"
                , "Zara"
                , "Maya"
                , "Baby"
                , "Bud"
                , "Ella"
                , "Ace"
                , "Kahlua"
                , "Jake"
                , "Apollo"
                , "Sammy"
                , "Puppy"
                , "Gucci"
                , "Mac"
                , "Belle"
                ]

            insensitiveStringContains : String -> String -> Bool
            insensitiveStringContains a b =
                String.contains (String.toLower a) (String.toLower b)

            choiceList : List String
            choiceList =
                if String.length lastChoices.query == 0 then
                    []

                else
                    List.filter (insensitiveStringContains lastChoices.query) dogs
        in
        Task.succeed { lastChoices | choices = choiceList }

-}
type alias Choices a =
    Internal.Choices a


type alias State a =
    { query : String
    , choices : List a
    , ignoreList : List a
    , viewStatus : ViewStatus
    , selectedIndex : Maybe Int
    , mouseDownIndex : Maybe Int
    , debounceState : Bounce.Bounce
    , minQueryLength : Int
    , inFocus : Bool
    }


{-| Record to expose common values of Autocomplete to be used for display
-}
type alias ViewState a =
    { query : String
    , choices : List a
    , ignoreList : List a
    , selectedIndex : Maybe Int
    , status : ViewStatus
    }


{-| A useful union type for rendering the correct view for each state of Autocomplete
-}
type ViewStatus
    = NotFetched
    | Fetching
    | Error String
    | FetchedChoices


{-| Initialize the Autocomplete state with your fetcher function

    init : ( Model, Cmd Msg )
    init =
        ( { -- Initialize the Autocomplete state
            autocompleteState = Autocomplete.init { query = "", choices = [], ignoreList = [] } fetcher
          , selectedValue = Nothing
          }
        , Cmd.none
        )

    fetcher : Autocomplete.Choices String -> Task String (Autocomplete.Choices String)
    fetcher lastChoices =
        let
            dogs =
                [ "Hunter"
                , "Polo"
                , "Loki"
                , "Angel"
                , "Scout"
                , "Lexi"
                , "Zara"
                , "Maya"
                , "Baby"
                , "Bud"
                , "Ella"
                , "Ace"
                , "Kahlua"
                , "Jake"
                , "Apollo"
                , "Sammy"
                , "Puppy"
                , "Gucci"
                , "Mac"
                , "Belle"
                ]

            insensitiveStringContains : String -> String -> Bool
            insensitiveStringContains a b =
                String.contains (String.toLower a) (String.toLower b)

            choiceList : List String
            choiceList =
                if String.length lastChoices.query == 0 then
                    []

                else
                    List.filter (insensitiveStringContains lastChoices.query) dogs
        in
        Task.succeed { lastChoices | choices = choiceList }

-}
init : Int -> Choices a -> Autocomplete a
init minQueryLength initChoices =
    Autocomplete
        { query = initChoices.query
        , choices = initChoices.choices
        , ignoreList = initChoices.ignoreList
        , viewStatus = NotFetched
        , selectedIndex = Nothing
        , mouseDownIndex = Nothing
        , debounceState = Bounce.init
        , minQueryLength = minQueryLength
        , inFocus = False
        }


{-| Updates the Autocomplete state


    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            -- This is the main wire-up to pass Autocomplete Msg to Autocomplete state
            OnAutocomplete autocompleteMsg ->
                let
                    ( newAutocompleteState, autoCompleteCmd ) =
                        Autocomplete.update autocompleteMsg model.autocompleteState
                in
                ( { model | autocompleteState = newAutocompleteState }
                , Cmd.map OnAutocomplete autoCompleteCmd
                )

    -- ...

-}
update : Msg -> Autocomplete a -> ( Autocomplete a, Bool, Cmd Msg )
update msg (Autocomplete state) =
    case msg of
        OnInput q ->
            ( Autocomplete
                { state
                    | query = q
                    , debounceState = Bounce.push state.debounceState
                    , selectedIndex = Nothing
                    , inFocus = True
                }
            , False
            , Bounce.delay 200 Debounce
            )

        OnBlur ->
            ( Autocomplete { state | inFocus = False }
            , False
            , Cmd.none
            )

        OnFocus ->
            ( Autocomplete { state | inFocus = True }
            , False
            , Cmd.none
            )

        Debounce ->
            let
                debounceState =
                    Bounce.pop state.debounceState

                doFetch =
                    Bounce.steady debounceState && state.minQueryLength <= String.length state.query
            in
            ( Autocomplete
                { state
                    | debounceState = debounceState
                    , viewStatus =
                        if doFetch then
                            Fetching

                        else
                            state.viewStatus
                }
            , doFetch
            , Cmd.none
            )

        OnKeyDown keyDown ->
            ( Autocomplete
                { state
                    | selectedIndex =
                        Internal.calculateIndex
                            (List.length state.choices)
                            state.selectedIndex
                            keyDown
                }
            , False
            , Cmd.none
            )

        OnMouseDown index ->
            ( Autocomplete { state | mouseDownIndex = Just index }, False, Cmd.none )

        OnMouseUp upIndex ->
            -- Check that mouse down and up have the same index
            -- otherwise ignore this event
            if state.mouseDownIndex == Just upIndex then
                ( Autocomplete
                    { state
                        | selectedIndex = Just upIndex
                        , mouseDownIndex = Nothing
                    }
                , False
                , Cmd.none
                )

            else
                ( Autocomplete state, False, Cmd.none )


{-|
Update the Autocomplete state with a search request result
-}
onFetch : Result String (Choices a) -> Autocomplete a -> Autocomplete a
onFetch result (Autocomplete state) =
    case result of
        Err s ->
            Autocomplete { state | viewStatus = Error s }

        Ok c ->
            if c.query == state.query && state.viewStatus == Fetching then
                Autocomplete
                    { state
                        | choices =
                            List.filter
                                (\i -> not <| List.member i c.ignoreList)
                                c.choices
                        , ignoreList = c.ignoreList
                        , viewStatus = FetchedChoices
                    }

            else
                Autocomplete state



-- Helpers


{-| Returns the selectedValue
-}
selectedValue : Autocomplete a -> Maybe a
selectedValue (Autocomplete s) =
    s.selectedIndex
        |> Maybe.map (\i -> List.drop i s.choices)
        |> Maybe.andThen List.head



-- Accessors


{-| Returns the ViewState of the Autocomplete to render the view.
Remember to attach Autocomplete events to your view!
See [Autocomplete.View](Autocomplete.View#events)
-}
viewState : Autocomplete a -> ViewState a
viewState (Autocomplete s) =
    { query = s.query
    , choices = s.choices
    , ignoreList = s.ignoreList
    , selectedIndex = s.selectedIndex
    , status = s.viewStatus
    }


{-| Returns the query of the Autocomplete
-}
query : Autocomplete a -> String
query (Autocomplete s) =
    s.query


{-| Returns the current list of choices
-}
choices : Autocomplete a -> List a
choices (Autocomplete s) =
    s.choices


{-|
Set the selected index
-}
setSelectedIndex : Int -> Autocomplete a -> Autocomplete a
setSelectedIndex i (Autocomplete s) =
    { s | selectedIndex = Just i }
        |> Autocomplete


{-|
Set the query
-}
setQuery : String -> Autocomplete a -> Autocomplete a
setQuery q (Autocomplete s) =
    { s | query = q }
        |> Autocomplete


{-|
Set the choices
-}
setChoices : List a -> Autocomplete a -> Autocomplete a
setChoices ch (Autocomplete s) =
    { s | choices = ch }
        |> Autocomplete


{-|
Set the view status
-}
setStatus : ViewStatus -> Autocomplete a -> Autocomplete a
setStatus st (Autocomplete s) =
    { s | viewStatus = st }
        |> Autocomplete


{-|
Get the focus state
-}
inFocus : Autocomplete a -> Bool
inFocus (Autocomplete s) =
    s.inFocus


{-|
Is the query string long enough for triggering a search request?
-}
enoughCharacters : Autocomplete s -> Bool
enoughCharacters (Autocomplete s) =
    String.length s.query >= s.minQueryLength


{-|
Is the message a blur message?
-}
isBlur : Msg -> Bool
isBlur msg =
    case msg of
        OnBlur ->
            True

        _ ->
            False



{-|
Is the message a focus message?
-}
isFocus : Msg -> Bool
isFocus msg =
    case msg of
        OnFocus ->
            True

        _ ->
            False


{-|
Is the message an input message?
-}
isInput : Msg -> Bool
isInput msg =
    case msg of
        OnInput _ ->
            True

        _ ->
            False
