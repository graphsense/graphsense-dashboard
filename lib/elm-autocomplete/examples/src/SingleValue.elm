module SingleValue exposing (main)

import Autocomplete exposing (Autocomplete)
import Autocomplete.View as AutocompleteView
import Browser
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Task exposing (Task)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { -- Add Autocomplete state to your model
      autocompleteState : Autocomplete String

    -- (Optional) final selected value from user
    , selectedValue : Maybe String
    }


type Msg
    = -- Autocomplete Msg
      OnAutocomplete (Autocomplete.Msg String)
      -- Your msg to be emitted when user selects a value
    | OnAutocompleteSelect
      -- (Optional) Your msg to be emitted on blur (to close autocomplete)
    | OnAutocompleteBlur


{-| Define your own fetcher function
which takes a `Autocomplete.Choices a`
and returns a `Task String (Autocomplete.Choices a)`.

    type alias Choices a =
        { query : String -- current query of the user
        , choices : List a -- previous list of choices
        , ignoreList : List a -- (optional) ignore list for cases like selected value
        }

The fetcher function is called by Autocomplete
whenever it needs to fetch new data with debouncing handled automatically.

-}
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



-- Model


init : ( Model, Cmd Msg )
init =
    ( { -- Initialize the Autocomplete state
        autocompleteState = Autocomplete.init { query = "", choices = [], ignoreList = [] } fetcher
      , selectedValue = Nothing
      }
    , Cmd.none
    )


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

        -- Optional msg to handle when user selects a choices
        OnAutocompleteSelect ->
            let
                { autocompleteState } =
                    model

                query =
                    Autocomplete.query autocompleteState

                selectedValue =
                    Autocomplete.selectedValue autocompleteState
            in
            ( { model
                -- Save the selectedValue into our own state
                | selectedValue = selectedValue

                -- Reset AutocompleteState
                , autocompleteState =
                    Autocomplete.reset
                        { query = Maybe.withDefault query selectedValue
                        , choices = []
                        , ignoreList = []
                        }
                        autocompleteState
              }
            , Cmd.none
            )

        -- Optional msg to handle when user lose focus on Autocomplete
        OnAutocompleteBlur ->
            let
                { autocompleteState } =
                    model

                query =
                    Autocomplete.query autocompleteState
            in
            ( { model
                | autocompleteState =
                    Autocomplete.reset
                        { query = query
                        , choices = []
                        , ignoreList = []
                        }
                        autocompleteState
              }
            , Cmd.none
            )



-- View


{-| Autocomplete does not provide a view renderer function
so we have to create one ourselves from the Autocomplete state
-}
view : Model -> Html Msg
view model =
    let
        { selectedValue, autocompleteState } =
            model

        -- Get view-related state from the Autocomplete State
        { query, choices, selectedIndex, status } =
            Autocomplete.viewState autocompleteState

        -- Important! We need to attach input and choice events to our view
        { inputEvents, choiceEvents } =
            AutocompleteView.events
                { onSelect = OnAutocompleteSelect
                , mapHtml = OnAutocomplete
                }
    in
    Html.div []
        [ Html.div [] [ Html.text <| "Selected Value: " ++ Maybe.withDefault "Nothing" selectedValue ]

        -- Our simple input view with the inputEvents from AutocompleteView.events
        -- which handles keydown/input events
        -- We add our own custom onBlur event to close the Autocomplete when focus is lost
        , Html.input
            (inputEvents
                ++ [ Html.Attributes.value query, Html.Events.onBlur OnAutocompleteBlur ]
            )
            []

        -- The container for our choices
        , Html.div [] <|
            -- Autocomplete.viewState provides a fetching status type
            -- We can use this to render our choices
            case status of
                Autocomplete.NotFetched ->
                    [ Html.text "" ]

                Autocomplete.Fetching ->
                    [ Html.text "Fetching..." ]

                Autocomplete.Error s ->
                    [ Html.text s ]

                Autocomplete.FetchedChoices ->
                    if String.length query > 0 then
                        -- Our simple div view for each choice with choiceEvent
                        -- from AutocompleteView.events which handles mouse click events
                        List.indexedMap (renderChoice choiceEvents selectedIndex) choices

                    else
                        [ Html.text "" ]
        ]


renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> String -> Html Msg
renderChoice events selectedIndex index s =
    Html.div
        (if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE" :: events index

         else
            Html.Attributes.style "backgroundColor" "#FFF" :: events index
        )
        [ Html.text s ]
