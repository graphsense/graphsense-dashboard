module View.Pathfinder.ContextMenuItem exposing (ContextMenuItem, init, init2, initLink2, map, rule, setDisabled, view)

import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.GraphComponents as HGraphComponents
import Util.Css
import View.Locale as Locale


type ContextMenuItem msg
    = ContextMenuItem (ContextMenuItemInternal msg)


type ContextMenuItemActions msg
    = ClickLink Bool String
    | ClickMsg msg


type alias ContextMenuItemInternal msg =
    { icon : Html msg
    , text1 : String
    , text2 : Maybe String
    , action : ContextMenuItemActions msg
    , disabled : Bool
    }


rule : View.Config -> Html msg
rule _ =
    Html.hr
        [ css
            [ Css.property "border" "none"
            , Css.property "background-color" Colors.grey50
            , Css.width (Css.pct 95)
            , Css.height (Css.px 1)
            , Css.marginTop (Css.px 3)
            , Css.marginBottom (Css.px 2)
            ]
        ]
        []


view : View.Config -> ContextMenuItem msg -> Html msg
view vc (ContextMenuItem { icon, text1, text2, action, disabled }) =
    let
        unsetFontStyle =
            if disabled then
                []

            else
                [ Css.fontWeight Css.unset, Css.color Css.unset ]
                    |> List.map Css.important
                    |> css
                    |> List.singleton

        ( msg, wrapper ) =
            if disabled then
                ( [], identity )

            else
                case action of
                    ClickLink blank link ->
                        ( []
                        , List.singleton
                            >> Html.a
                                ([ Html.Styled.Attributes.href link
                                 , [ Css.textDecoration Css.none, Css.color Css.inherit, Css.visited [ Css.color Css.inherit, Css.textDecoration Css.none ] ] ++ fullWidth |> css
                                 ]
                                    ++ (if blank then
                                            [ Html.Styled.Attributes.target "_blank"
                                            ]

                                        else
                                            []
                                       )
                                )
                        )

                    ClickMsg m ->
                        ( [ onClick m ], identity )
    in
    HGraphComponents.rightClickItemWithAttributes
        (HGraphComponents.rightClickItemAttributes
            |> Rs.s_root
                (((HGraphComponents.rightClickItemStateHoverTypeWithIcon_details.styles
                    ++ HGraphComponents.rightClickItemStateHoverTypeWithIconPlaceholder1_details.styles
                    ++ fullWidth
                    |> Css.hover
                  )
                    :: fullWidth
                    ++ (if disabled then
                            [ Util.Css.overrideBlack Colors.greyBlue100
                            ]

                        else
                            [ Css.cursor Css.pointer
                            ]
                       )
                    |> css
                 )
                    :: msg
                )
            |> Rs.s_placeholder1
                unsetFontStyle
            |> Rs.s_placeholder2
                unsetFontStyle
            |> Rs.s_iconsDividerNoPadding [ [ Css.position Css.relative ] |> css ]
        )
        { root =
            { iconInstance = icon
            , text1 = Locale.string vc.locale text1
            , text2 = Locale.string vc.locale (text2 |> Maybe.withDefault "")
            , text2Visible = Maybe.Extra.isJust text2
            , state =
                if disabled then
                    HGraphComponents.RightClickItemStateDisabled

                else
                    HGraphComponents.RightClickItemStateNeutral
            , type_ = HGraphComponents.RightClickItemTypeWithIcon
            }
        }
        |> wrapper


map : (a -> b) -> ContextMenuItem a -> ContextMenuItem b
map mp (ContextMenuItem { icon, text1, text2, action, disabled }) =
    ContextMenuItem
        { icon = Html.map mp icon
        , text1 = text1
        , text2 = text2
        , action =
            case action of
                ClickMsg msg ->
                    ClickMsg (mp msg)

                ClickLink blank l ->
                    ClickLink blank l
        , disabled = disabled
        }


init :
    { icon : Html msg
    , text : String
    , msg : msg
    }
    -> ContextMenuItem msg
init { icon, text, msg } =
    init2
        { icon = icon
        , text1 = text
        , text2 = Nothing
        , msg = msg
        }


init2 :
    { icon : Html msg
    , text1 : String
    , text2 : Maybe String
    , msg : msg
    }
    -> ContextMenuItem msg
init2 { icon, text1, text2, msg } =
    ContextMenuItem
        { icon = icon
        , text1 = text1
        , text2 = text2
        , action = ClickMsg msg
        , disabled = False
        }


initLink2 :
    { icon : Html msg
    , text1 : String
    , text2 : Maybe String
    , link : String
    , blank : Bool
    }
    -> ContextMenuItem msg
initLink2 { icon, text1, text2, blank, link } =
    ContextMenuItem
        { icon = icon
        , text1 = text1
        , text2 = text2
        , action = ClickLink blank link
        , disabled = False
        }


setDisabled : Bool -> ContextMenuItem msg -> ContextMenuItem msg
setDisabled disabled (ContextMenuItem item) =
    ContextMenuItem
        { item | disabled = disabled }
