module AsyncFetcher exposing (main)

import Autocomplete exposing (Autocomplete, choices)
import Autocomplete.View as AutocompleteView
import Browser
import Html exposing (Attribute, Html)
import Html.Attributes
import Http
import Json.Decode as JD
import Json.Encode as JE
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
    { autocompleteState : Autocomplete Post
    , selectedValue : Maybe Post
    }


type alias Post =
    { id : Int
    , title : String
    , body : String
    }


type Msg
    = OnAutocomplete (Autocomplete.Msg Post)
    | OnAutocompleteSelect


postDecoder : JD.Decoder Post
postDecoder =
    JD.map3 Post
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "body" JD.string)


localFilter : Autocomplete.Choices Post -> List Post -> Autocomplete.Choices Post
localFilter lastChoices posts =
    let
        insensitiveStringContains : String -> String -> Bool
        insensitiveStringContains a b =
            String.contains (String.toLower a) (String.toLower b)
    in
    { lastChoices | choices = List.filter (.title >> insensitiveStringContains lastChoices.query) posts }


resolver : Autocomplete.Choices Post -> Http.Resolver String (Autocomplete.Choices Post)
resolver lastChoices =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ s ->
                    Err <| "Bad url: " ++ s

                Http.Timeout_ ->
                    Err "Request timeout"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadStatus_ _ s ->
                    Err <| "Bad status: " ++ s

                Http.GoodStatus_ _ body ->
                    JD.decodeString (JD.list postDecoder) body
                        -- We are using API from https://jsonplaceholder.typicode.com/posts - Which do not have filtering feature
                        -- So that in order to do demo, we will filter the result after we receive the data from API
                        |> Result.map (localFilter lastChoices)
                        |> Result.mapError (\_ -> "Decode error")
        )


fetcher : Autocomplete.Choices Post -> Task String (Autocomplete.Choices Post)
fetcher lastChoices =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://jsonplaceholder.typicode.com/posts"
        , body = Http.stringBody "application/json" (JE.encode 0 <| JE.string lastChoices.query)
        , timeout = Nothing
        , resolver = resolver lastChoices
        }



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
                        { query = Maybe.withDefault query <| Maybe.map .title selectedValue
                        , choices = []
                        , ignoreList = Maybe.withDefault [] <| Maybe.map List.singleton selectedValue
                        }
                        autocompleteState
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    let
        { selectedValue, autocompleteState } =
            model

        { query, choices, selectedIndex, status } =
            Autocomplete.viewState autocompleteState

        { inputEvents, choiceEvents } =
            AutocompleteView.events
                { onSelect = OnAutocompleteSelect
                , mapHtml = OnAutocomplete
                }
    in
    Html.div []
        [ Html.div []
            [ Html.text <|
                "Selected Value: "
                    ++ (Maybe.map .title selectedValue |> Maybe.withDefault "Nothing")
            ]
        , Html.input (inputEvents ++ [ Html.Attributes.value query ]) []
        , Html.div [] <|
            case status of
                Autocomplete.NotFetched ->
                    [ Html.text "" ]

                Autocomplete.Fetching ->
                    [ Html.text "Fetching..." ]

                Autocomplete.Error s ->
                    [ Html.text s ]

                Autocomplete.FetchedChoices ->
                    List.indexedMap (renderChoice choiceEvents selectedIndex) choices
        ]


renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> Post -> Html Msg
renderChoice events selectedIndex index post =
    Html.div
        (if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE" :: events index

         else
            Html.Attributes.style "backgroundColor" "#FFF" :: events index
        )
        [ Html.text post.title ]
