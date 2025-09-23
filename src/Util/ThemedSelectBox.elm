module Util.ThemedSelectBox exposing
    ( Config
    , Model
    , Msg(..)
    , OutMsg(..)
    , close
    , defaultConfig
    , empty
    , getOptions
    , init
    , update
    , view
    , viewDisabled
    , viewWithLabel
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
import Util.View


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
    { optionToLabel = optionToLabel, width = Nothing, filter = always True }


type alias Config a b =
    { optionToLabel : a -> String
    , width : Maybe (Css.ExplicitLength b)
    , filter : a -> Bool
    }


viewWithLabel : Config a b -> Model a -> a -> String -> Html (Msg a)
viewWithLabel config m selected label =
    F.dropDownLabel { dropDown = { variant = view config m selected }, root = { label = label } }


getWidthAttrs : Config a b -> Html.Styled.Attribute msg
getWidthAttrs config =
    case config.width of
        Just w ->
            [ Css.width w |> Css.important ] |> css

        _ ->
            [ Css.width (Css.px Sc.dropDownClosed_details.width) |> Css.important ] |> css


viewDisabled : Config a b -> Model a -> a -> Html (Msg a)
viewDisabled config _ selected =
    let
        baseAttrs =
            [ getWidthAttrs config ]
    in
    F.dropDownStateDisabledWithAttributes
        (F.dropDownStateDisabledAttributes
            |> Rs.s_root baseAttrs
            |> Rs.s_text (([ Css.alignItems Css.center ] |> css) :: baseAttrs)
        )
        { root =
            { iconInstance = Icons.iconsChevronDownThick {}
            , text = config.optionToLabel selected
            }
        }


view : Config a b -> Model a -> a -> Html (Msg a)
view config (SelectBox sBox) selected =
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
            Sc.dropDownLabelsWithAttributes
                (Sc.dropDownLabelsAttributes
                    |> Rs.s_root itemAttributes
                )
                { root =
                    { state =
                        if Just x == sItem then
                            Sc.DropDownLabelsStateActive

                        else
                            Sc.DropDownLabelsStateNeutral
                    , size = Sc.DropDownLabelsSizeNormal
                    , dropDownText = config.optionToLabel x
                    }
                }

        selectedLabel =
            selectedItem
                |> Maybe.map config.optionToLabel
                |> Maybe.withDefault ""
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
        Sc.dropDownOpenWithAttributes
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
            { dropDownList = dropDownList
            }
            { dropDownHeaderOpen =
                { text = selectedLabel
                }
            }

    else
        Sc.dropDownClosedWithAttributes
            (Sc.dropDownClosedAttributes
                |> Rs.s_root
                    [ Util.View.onClickWithStop Open
                    , Util.View.pointer
                    , widthAttr
                    ]
            )
            { root = { text = selectedLabel } }
