module View.Search exposing (SearchConfigWithMoreCss, default, searchWithMoreCss)

import Autocomplete
import Autocomplete.Styled as Autocomplete
import Config.View exposing (Config)
import Css exposing (Style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import List.Extra
import Model.Search exposing (..)
import Msg.Search exposing (Msg(..))
import Plugin.View as Plugin
import RecordSetter as Rs
import String.Extra
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.SearchComponents as SearchComponents
import Theme.Html.SettingsComponents as SettingsComponents
import Util exposing (removeLeading0x)
import Util.Data as Data
import Util.View exposing (fullWidthCss, pointer)
import Util.View.Loadingspinner as Loadingspinner
import View.Autocomplete as Autocomplete
import View.Locale as Locale


type alias SearchConfigWithMoreCss msg =
    { formCss : List Style
    , frameCss : List Style
    , button : List Style
    , resultLine : List Style
    , resultLineHighlighted : List Style
    , resultGroup : List Style
    , resultGroupTitle : List Style
    , resultLineIcon : List Style
    , resultTextEmphasized : List Style
    , dropdownFrame : List Style
    , dropdownResult : List Style
    , multiline : Bool
    , inputAttributes : List (Html.Styled.Attribute msg)
    }


default : SearchConfigWithMoreCss msg
default =
    { multiline = False
    , formCss = []
    , frameCss = []
    , button = []
    , resultLine = []
    , resultLineHighlighted = []
    , resultGroup = []
    , resultGroupTitle = []
    , resultLineIcon = []
    , resultTextEmphasized = []
    , dropdownFrame = []
    , dropdownResult = []
    , inputAttributes = []
    }


searchWithMoreCss : Config -> SearchConfigWithMoreCss Msg -> Model -> Html Msg
searchWithMoreCss vc sc model =
    let
        { inputEvents } =
            Autocomplete.events
                { onSelect = UserClicksResultLine
                , mapHtml = AutocompleteMsg
                }

        { query } =
            Autocomplete.viewState model.autocomplete
    in
    Html.Styled.form
        [ css
            [ Css.flexGrow <| Css.num 1
            , Css.height Css.auto |> Css.important
            ]
        , css sc.formCss
        , stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True ))
        , onSubmit UserClicksResultLine
        ]
        [ div
            [ --Css.frame vc |> css
              css
                [ Css.height <| Css.pct 100
                , Css.marginRight Css.zero |> Css.important
                ]
            , css sc.frameCss
            ]
            [ input
                ([ css
                    (Css.outline Css.none
                        :: Css.pseudoClass "placeholder" SettingsComponents.searchBarFieldStatePlaceholderSearchInputField_details.styles
                        :: (Css.width <| Css.pct 100)
                        :: SettingsComponents.searchBarFieldStateTypingSearchInputField_details.styles
                        ++ SettingsComponents.searchBarFieldStateTypingSearchText_details.styles
                    )
                 , autocomplete False
                 , spellcheck False
                 , Locale.string vc.locale "The search" |> title
                 , onBlur UserLeavesSearch
                 , onFocus UserFocusSearch
                 , on "mousedown" (Json.Decode.succeed UserInputPressed)
                 , value query
                 , id searchInputId
                 ]
                    ++ inputEvents
                    ++ (case model.searchType of
                            SearchAll _ ->
                                [ "Address", "transaction", "label", "block", "actor" ]
                                    |> List.map (Locale.string vc.locale)
                                    |> (\st -> st ++ Plugin.searchPlaceholder vc)
                                    |> String.join ", "
                                    |> placeholder
                                    |> List.singleton

                            SearchAddressAndTx _ ->
                                [ "Address", "transaction" ]
                                    |> List.map (Locale.string vc.locale)
                                    |> (\st -> st ++ Plugin.searchPlaceholder vc)
                                    |> String.join ", "
                                    |> placeholder
                                    |> List.singleton

                            SearchTagsOnly ->
                                [ Locale.string vc.locale "Label"
                                    |> placeholder
                                ]

                            SearchActorsOnly ->
                                [ Locale.string vc.locale "actor"
                                    |> placeholder
                                ]
                       )
                    ++ sc.inputAttributes
                )
                []
            , searchResult vc sc model
            ]
        ]


searchResult : Config -> SearchConfigWithMoreCss Msg -> Model -> Html Msg
searchResult vc sc model =
    let
        viewState =
            Autocomplete.viewState model.autocomplete

        isLoading =
            viewState.status == Autocomplete.Fetching

        config1 =
            { frame = Css.property "background-color" Colors.white :: sc.dropdownFrame
            , result = Css.property "background-color" Colors.white :: sc.dropdownResult
            , loadingSpinner =
                Loadingspinner.html
                    [ css
                        [ Css.position Css.absolute
                        , Css.top Css.zero
                        , Css.right Css.zero
                        ]
                    ]
            }

        config2 =
            { loading = isLoading
            , visible = model.visible
            , onClick = NoOp
            }

        min_search_length =
            minSearchLengthWithResultExpected model.searchType

        noResults =
            (viewState.choices |> List.isEmpty) && viewState.status == Autocomplete.FetchedChoices

        lengthOfMutliInput =
            Data.parseMultiIdentifierInput viewState.query
                |> List.length

        msg =
            text
                >> List.singleton
                >> div
                    [ css [ Css.paddingBottom <| Css.px 1 ] ]
                >> List.singleton
    in
    if String.isEmpty viewState.query && model.visible && not (List.isEmpty (filteredRecents model.searchType model.recentSearches)) then
        recentList vc sc model
            |> Autocomplete.dropdownStyled
                config1
                vc
                config2

    else if (viewState.query |> removeLeading0x |> String.length) < min_search_length && model.visible then
        msg (Locale.interpolated vc.locale "Hint-minimum-input" [ String.fromInt min_search_length ])
            |> Autocomplete.dropdownStyled
                config1
                vc
                config2

    else if (viewState.query |> removeLeading0x |> String.length) > 0 && model.visible && noResults then
        msg (Locale.string vc.locale "No-results-found")
            |> Autocomplete.dropdownStyled
                config1
                vc
                { loading = isLoading
                , visible = True
                , onClick = NoOp
                }

    else if (lengthOfMutliInput > 1) && model.visible then
        msg (Locale.interpolated vc.locale "Hint-multiple-search-terms" [ String.fromInt lengthOfMutliInput ])
            |> Autocomplete.dropdownStyled
                config1
                vc
                config2

    else if model.visible then
        resultList vc sc model
            |> Autocomplete.dropdownStyled
                config1
                vc
                config2

    else
        text ""


type alias Badge =
    { title : String
    , badge : List ( Int, ResultLine )
    }


{-| Group result lines (autocomplete choices or recent searches) into titled
badges: one badge per currency (BTC, ETH, …) plus separate badges for actors
and labels, depending on the search type.
-}
groupBadges : Config -> SearchType -> List ( Int, ResultLine ) -> List Badge
groupBadges vc searchType choices =
    let
        filterChoices pred =
            List.filter (Tuple.second >> pred) choices

        labelBadge =
            { title = Locale.string vc.locale "Labels"
            , badge =
                filterChoices
                    (\rl ->
                        case rl of
                            Label _ ->
                                True

                            _ ->
                                False
                    )
            }

        actorBadge =
            { title = Locale.string vc.locale "actors"
            , badge =
                filterChoices
                    (\rl ->
                        case rl of
                            Actor _ ->
                                True

                            Custom _ ->
                                True

                            _ ->
                                False
                    )
            }

        currencyBadges =
            choices
                |> List.Extra.gatherEqualsBy (Tuple.second >> resultLineCurrency)
                |> List.filterMap
                    (\( fst, rest ) ->
                        Tuple.second fst
                            |> resultLineCurrency
                            |> Maybe.map
                                (\cur ->
                                    { title = String.toUpper cur
                                    , badge = fst :: rest
                                    }
                                )
                    )
    in
    case searchType of
        SearchTagsOnly ->
            [ labelBadge ]

        SearchActorsOnly ->
            [ actorBadge ]

        SearchAddressAndTx _ ->
            currencyBadges

        SearchAll _ ->
            currencyBadges ++ [ actorBadge ]


badgeToResult : Config -> SearchConfigWithMoreCss Msg -> (( Int, ResultLine ) -> Html Msg) -> Bool -> Badge -> Html Msg
badgeToResult _ sc renderLine showTitle { title, badge } =
    SearchComponents.autocompleteGroupWithAttributes
        (SearchComponents.autocompleteGroupAttributes
            |> Rs.s_root [ css (fullWidthCss :: sc.resultGroup) ]
            |> Rs.s_btc (css sc.resultGroupTitle :: Util.View.conditionalHide showTitle)
        )
        { rowList =
            badge
                |> List.map renderLine
        }
        { root =
            { label = title
            }
        }


{-| Render the non-empty badges as result groups. When only a single group
remains, its title is omitted since the grouping then carries no information.
-}
renderBadges : Config -> SearchConfigWithMoreCss Msg -> (( Int, ResultLine ) -> Html Msg) -> List Badge -> List (Html Msg)
renderBadges vc sc renderLine badges =
    let
        nonEmpty =
            List.filter (.badge >> List.isEmpty >> not) badges
    in
    nonEmpty
        |> List.map (badgeToResult vc sc renderLine (List.length nonEmpty > 1))


recentList : Config -> SearchConfigWithMoreCss Msg -> Model -> List (Html Msg)
recentList vc sc model =
    let
        recents =
            filteredRecents model.searchType model.recentSearches
                |> List.indexedMap Tuple.pair

        lineEvents rl =
            [ preventDefaultOn "mousedown" (Json.Decode.succeed ( NoOp, True ))
            , onClick (UserClicksRecentResultLine rl)
            ]

        renderLine ( _, rl ) =
            resultLineToHtml "" sc Nothing (lineEvents rl) rl

        groups =
            groupBadges vc model.searchType recents
                |> renderBadges vc sc renderLine
    in
    if List.isEmpty recents then
        []

    else
        [ SearchComponents.autocompleteGroupWithAttributes
            (SearchComponents.autocompleteGroupAttributes
                |> Rs.s_root [ css (fullWidthCss :: sc.resultGroup) ]
                |> Rs.s_btc [ css sc.resultGroupTitle ]
            )
            { rowList = groups
            }
            { root = { label = Locale.string vc.locale "Recent searches" } }
        ]


resultList : Config -> SearchConfigWithMoreCss Msg -> Model -> List (Html Msg)
resultList vc sc { autocomplete, searchType } =
    let
        choices =
            Autocomplete.choices autocomplete
                |> List.indexedMap Tuple.pair

        q =
            (Autocomplete.viewState autocomplete).query

        { choiceEvents } =
            Autocomplete.events
                { onSelect = UserClicksResultLine
                , mapHtml = AutocompleteMsg
                }

        selectedValue =
            Autocomplete.selectedValue autocomplete

        renderLine ( index, rl ) =
            resultLineToHtml q sc selectedValue (choiceEvents index) rl
    in
    groupBadges vc searchType choices
        |> renderBadges vc sc renderLine



--++ Plugin.searchResultList plugins pluginStates vc


resultLineToHtml : String -> SearchConfigWithMoreCss Msg -> Maybe ResultLine -> List (Attribute Msg) -> ResultLine -> Html Msg
resultLineToHtml query sc selectedValue choiceEvents resultLine =
    let
        ( icon, label, highlight_suffix ) =
            let
                search_prefix_length =
                    Basics.max 8 (String.length (removeLeading0x query) + 2)
            in
            case resultLine of
                Address _ a ->
                    ( Icons.iconsAddress {}
                    , Util.View.truncateLongIdentifierWithLengths search_prefix_length 4 a
                      -- Util.View.truncate 50 a
                    , True
                    )

                Tx _ a ->
                    ( Icons.iconsTransaction {}
                    , Util.View.truncateLongIdentifierWithLengths search_prefix_length 8 a
                      -- Util.View.truncate 70 a
                    , True
                    )

                Block _ a ->
                    ( Icons.iconsCopyS {}
                    , String.fromInt a
                    , True
                    )

                Label a ->
                    ( Icons.iconsTagS {}, a, False )

                Actor ( _, lbl ) ->
                    ( Icons.iconsActor {}, lbl, True )

                Custom x ->
                    ( Icons.iconsPlusSnoPadding {}
                    , x.label
                    , False
                    )

        querycomp =
            removeLeading0x query

        ( regularText, boldText ) =
            if not (String.isEmpty querycomp) && String.startsWith querycomp (removeLeading0x label) && highlight_suffix then
                let
                    left =
                        String.Extra.leftOf querycomp label

                    right =
                        String.Extra.rightOf querycomp label

                    total =
                        left ++ querycomp ++ right
                in
                if total /= label then
                    ( label, "" )

                else
                    ( left ++ querycomp
                    , right
                    )

            else
                ( label, "" )
    in
    SearchComponents.autocompleteRowWithAttributes
        (SearchComponents.autocompleteRowAttributes
            |> Rs.s_root
                (css
                    [ Css.hover SearchComponents.autocompleteRowHighlightedTrue_details.styles
                    , fullWidthCss
                    ]
                    :: css sc.resultLine
                    :: pointer
                    :: choiceEvents
                )
        )
        { root =
            { highlighted =
                if selectedValue == Just resultLine then
                    SearchComponents.AutocompleteRowHighlightedTrue

                else
                    SearchComponents.AutocompleteRowHighlightedFalse
            , regularText = regularText
            , boldText = boldText
            , icon =
                div
                    [ css sc.resultLineIcon
                    ]
                    [ icon
                    ]
            }
        }


resultLineCurrency : ResultLine -> Maybe String
resultLineCurrency rl =
    case rl of
        Address currency _ ->
            Just currency

        Tx currency _ ->
            Just currency

        Block currency _ ->
            Just currency

        Label _ ->
            Nothing

        Actor _ ->
            Nothing

        Custom _ ->
            Nothing
