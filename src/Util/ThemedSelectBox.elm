module Util.ThemedSelectBox exposing
    ( Config
    , Model
    , Msg(..)
    , OutMsg(..)
    , close
    , defaultConfig
    , defaultConfigHtml
    , empty
    , getOptions
    , init
    , update
    , updateOptions
    , view
    , viewDisabled
    , viewWithLabel
    , withFilter
    )

import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onMouseLeave)
import List.Extra
import RecordSetter as Rs
import Theme.Html.Fields as F
import Theme.Html.Icons as Icons
import Theme.Html.SelectionControls as Sc
import Util.Css
import Util.View exposing (none)


type Msg a
    = Select a
    | Open
    | Close


type OutMsg a
    = Selected a
    | NoSelection


type Model a
    = SelectBox (SelectBoxModel a)


type alias SelectBoxModel a =
    { options : List a
    , open : Bool
    }


init : List a -> Model a
init options =
    SelectBox
        { options = options
        , open = False
        }


updateOptions : List a -> Model a -> Model a
updateOptions options (SelectBox m) =
    SelectBox { m | options = options }


getOptions : Model a -> List a
getOptions (SelectBox m) =
    m.options


empty : Model a
empty =
    SelectBox { options = [], open = False }


close : Model a -> Model a
close (SelectBox m) =
    { m | open = False } |> SelectBox


open : Model a -> Model a
open (SelectBox m) =
    { m | open = True }
        |> SelectBox


select : Model a -> Model a
select (SelectBox m) =
    { m | open = not m.open }
        |> SelectBox


update : Msg a -> Model a -> ( Model a, OutMsg a )
update msg model =
    case msg of
        Select x ->
            ( select model
            , Selected x
            )

        Open ->
            ( open model, NoSelection )

        Close ->
            ( close model, NoSelection )


defaultConfig : (a -> String) -> Config a
defaultConfig optionToLabel =
    Config
        { optionToLabel = optionToLabel >> Html.Styled.text, filter = always True }


defaultConfigHtml : (a -> Html.Styled.Html (Msg a)) -> Config a
defaultConfigHtml optionToLabel =
    Config
        { optionToLabel = optionToLabel, filter = always True }


type Config a
    = Config (ConfigInternal a)


type alias ConfigInternal a =
    { optionToLabel : a -> Html.Styled.Html (Msg a)
    , filter : a -> Bool
    }


viewWithLabel : Config a -> List (Html.Styled.Attribute (Msg a)) -> Model a -> a -> String -> Html (Msg a)
viewWithLabel config attrs m selected label =
    F.dropDownLabel { dropDown = { variant = view config attrs m selected }, root = { label = label } }


viewDisabled : Config a -> List (Html.Styled.Attribute (Msg a)) -> Model a -> a -> Html (Msg a)
viewDisabled (Config config) attrs _ selected =
    let
        baseAttrs =
            attrs
    in
    F.dropDownStateDisabledWithInstances
        (F.dropDownStateDisabledAttributes
            |> Rs.s_root baseAttrs
            |> Rs.s_text (([ Css.alignItems Css.center ] |> css) :: baseAttrs)
        )
        (F.dropDownStateDisabledInstances
            |> Rs.s_text (config.optionToLabel selected |> Just)
        )
        { root =
            { iconInstance = Icons.iconsChevronDownThick {}
            , text = ""
            }
        }


view : Config a -> List (Html.Styled.Attribute (Msg a)) -> Model a -> a -> Html (Msg a)
view (Config config) attrs (SelectBox sBox) selected =
    let
        selectedItem =
            List.Extra.find ((==) selected) sBox.options

        createRow sItem hoverEffect x =
            let
                itemAttributes =
                    [ Css.cursor Css.pointer
                        :: (Css.width (Css.pct 100) |> Css.important)
                        :: Css.property "user-select" "none"
                        :: (if hoverEffect then
                                [ Css.hover <|
                                    (Css.pct 100 |> Css.width |> Css.important)
                                        :: Sc.dropDownLabelsStateHoverSizeNormal_details.styles
                                ]

                            else
                                []
                           )
                        |> css
                    , Util.View.onClickWithStop (Select x)
                    ]
                        ++ attrs
            in
            Sc.dropDownLabelsWithInstances
                (Sc.dropDownLabelsAttributes
                    |> Rs.s_root itemAttributes
                 --|> Rs.s_label [ css [ Css.height Css.auto ] ]
                )
                (Sc.dropDownLabelsInstances
                    |> Rs.s_label (Just <| config.optionToLabel x)
                )
                { root =
                    { state =
                        if Just x == sItem then
                            Sc.DropDownLabelsStateActive

                        else
                            Sc.DropDownLabelsStateNeutral
                    , size = Sc.DropDownLabelsSizeNormal
                    , dropDownText = ""
                    }
                }

        selectedLabel =
            selectedItem
                |> Maybe.map config.optionToLabel
                |> Maybe.withDefault none
    in
    if sBox.open then
        let
            dropdownOverlayCss =
                [ Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1))
                , Css.property "user-select" "none"
                , Css.height Css.auto
                ]

            dropDownList =
                sBox.options
                    |> List.filter config.filter
                    |> List.map (createRow selectedItem True)
        in
        Sc.dropDownOpenWithInstances
            (Sc.dropDownOpenAttributes
                |> Rs.s_root
                    ([ Util.View.onClickWithStop Close
                     , onMouseLeave Close
                     , Util.View.pointer
                     ]
                        ++ attrs
                    )
                |> Rs.s_dropDownList (css dropdownOverlayCss :: attrs)
                |> Rs.s_dropDownHeaderOpen attrs
             --|> Rs.s_text [ css [ Css.height Css.auto ] ]
            )
            (Sc.dropDownOpenInstances
                |> Rs.s_text (Just selectedLabel)
            )
            { dropDownList = dropDownList
            }
            { dropDownHeaderOpen =
                { text = ""
                }
            }

    else
        Sc.dropDownClosedWithInstances
            (Sc.dropDownClosedAttributes
                |> Rs.s_root
                    ([ Util.View.onClickWithStop Open
                     , Util.View.pointer
                     ]
                        ++ attrs
                    )
            )
            (Sc.dropDownClosedInstances
                |> Rs.s_text (Just selectedLabel)
            )
            { root = { text = "" } }


withFilter : (a -> Bool) -> Config a -> Config a
withFilter filter (Config config) =
    Config { config | filter = filter }
