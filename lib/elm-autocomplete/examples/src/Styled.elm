module Styled exposing (main)

import Autocomplete exposing (Autocomplete)
import Autocomplete.Styled as AutocompleteStyled
import Browser
import Html.Styled as StyledHtml
import Html.Styled.Attributes as StyledAttributes
import Task exposing (Task)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view >> StyledHtml.toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { autocompleteState : Autocomplete String
    , selectedValue : Maybe String
    }


type Msg
    = OnAutocomplete (Autocomplete.Msg String)
    | OnAutocompleteSelect


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
    ( { autocompleteState = Autocomplete.init { query = "", choices = [], ignoreList = [] } fetcher
      , selectedValue = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAutocomplete autocompleteMsg ->
            let
                ( newAutocompleteState, autoCompleteCmd ) =
                    Autocomplete.update autocompleteMsg model.autocompleteState
            in
            ( { model | autocompleteState = newAutocompleteState }
            , Cmd.map OnAutocomplete autoCompleteCmd
            )

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
                | selectedValue = selectedValue
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



-- View


view : Model -> StyledHtml.Html Msg
view model =
    let
        { selectedValue, autocompleteState } =
            model

        { query, choices, selectedIndex, status } =
            Autocomplete.viewState autocompleteState

        { inputEvents, choiceEvents } =
            AutocompleteStyled.events
                { onSelect = OnAutocompleteSelect
                , mapHtml = OnAutocomplete
                }
    in
    StyledHtml.div []
        [ StyledHtml.div [] [ StyledHtml.text <| "Selected Value: " ++ Maybe.withDefault "Nothing" selectedValue ]
        , StyledHtml.input (inputEvents ++ [ StyledAttributes.value query ]) []
        , StyledHtml.div [] <|
            case status of
                Autocomplete.NotFetched ->
                    [ StyledHtml.text "" ]

                Autocomplete.Fetching ->
                    [ StyledHtml.text "Fetching..." ]

                Autocomplete.Error s ->
                    [ StyledHtml.text s ]

                Autocomplete.FetchedChoices ->
                    if String.length query > 0 then
                        List.indexedMap (renderChoice choiceEvents selectedIndex) choices

                    else
                        [ StyledHtml.text "" ]
        ]


renderChoice : (Int -> List (StyledHtml.Attribute Msg)) -> Maybe Int -> Int -> String -> StyledHtml.Html Msg
renderChoice events selectedIndex index s =
    StyledHtml.div
        (if Autocomplete.isSelected selectedIndex index then
            StyledAttributes.style "backgroundColor" "#EEE" :: events index

         else
            StyledAttributes.style "backgroundColor" "#FFF" :: events index
        )
        [ StyledHtml.text s ]
