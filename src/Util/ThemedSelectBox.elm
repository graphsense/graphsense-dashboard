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
    , withWidth
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


defaultConfig : (a -> String) -> Config a b
defaultConfig optionToLabel =
    Config
        { optionToLabel = optionToLabel >> Html.Styled.text, width = Nothing, filter = always True }


withWidth : Css.ExplicitLength b -> Config a b -> Config a b
withWidth width (Config config) =
    Config { config | width = Just width }


defaultConfigHtml : (a -> Html.Styled.Html (Msg a)) -> Config a b
defaultConfigHtml optionToLabel =
    Config
        { optionToLabel = optionToLabel, width = Nothing, filter = always True }


type Config a b
    = Config (ConfigInternal a b)


type alias ConfigInternal a b =
    { optionToLabel : a -> Html.Styled.Html (Msg a)
    , width : Maybe (Css.ExplicitLength b)
    , filter : a -> Bool
    }


viewWithLabel : Config a b -> Model a -> a -> String -> Html (Msg a)
viewWithLabel config m selected label =
    F.dropDownLabel { dropDown = { variant = view config m selected }, root = { label = label } }


getWidthAttrs : ConfigInternal a b -> Html.Styled.Attribute (Msg a)
getWidthAttrs config =
    case config.width of
        Just w ->
            [ Css.width w |> Css.important ] |> css

        _ ->
            [ Css.width (Css.px Sc.dropDownClosed_details.width) |> Css.important ] |> css


viewDisabled : Config a b -> Model a -> a -> Html (Msg a)
viewDisabled (Config config) _ selected =
    let
        baseAttrs =
            [ getWidthAttrs config ]
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


view : Config a b -> Model a -> a -> Html (Msg a)
view (Config config) (SelectBox sBox) selected =
    let
        selectedItem =
            List.Extra.find ((==) selected) sBox.options

        widthAttr =
            getWidthAttrs config

        createRow sItem hoverEffect x =
            let
                itemAttributes =
                    [ Css.cursor Css.pointer
                        :: Css.property "user-select" "none"
                        :: (if hoverEffect then
                                [ Css.hover Sc.dropDownLabelsStateHoverSizeNormal_details.styles ]

                            else
                                []
                           )
                        |> css
                    , Util.View.onClickWithStop (Select x)
                    , widthAttr
                    ]
            in
            Sc.dropDownLabelsWithInstances
                (Sc.dropDownLabelsAttributes
                    |> Rs.s_root itemAttributes
                )
                (Sc.dropDownLabelsInstances
                    |> Rs.s_subtitle1 (Just <| config.optionToLabel x)
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
                [ Css.position Css.absolute
                , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1))
                , Css.top (Css.px Sc.dropDownClosed_details.height)
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
                    [ Util.View.onClickWithStop Close
                    , onMouseLeave Close
                    , Util.View.pointer
                    , css
                        [ Sc.dropDownClosed_details.height
                            |> Css.px
                            |> Css.height
                            |> Css.important
                        ]
                    , widthAttr
                    ]
                |> Rs.s_dropDownList [ css dropdownOverlayCss, widthAttr ]
                |> Rs.s_dropDownHeaderOpen [ widthAttr ]
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
                    [ Util.View.onClickWithStop Open
                    , Util.View.pointer
                    , widthAttr
                    ]
            )
            (Sc.dropDownClosedInstances
                |> Rs.s_text (Just selectedLabel)
            )
            { root = { text = "" } }


withFilter : (a -> Bool) -> Config a b -> Config a b
withFilter filter (Config config) =
    Config { config | filter = filter }
