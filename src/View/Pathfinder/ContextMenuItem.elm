module View.Pathfinder.ContextMenuItem exposing (ContextMenuItem, init, init2, initLink2, map, view)

import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra
import RecordSetter as Rs
import Theme.Html.GraphComponents as HGraphComponents
import View.Locale as Locale


type ContextMenuItem msg
    = ContextMenuItem (ContextMenuItemInternal msg)


type ContextMenuItemActions msg
    = ClickLink String
    | ClickMsg msg


type alias ContextMenuItemInternal msg =
    { icon : Html msg
    , text1 : String
    , text2 : Maybe String
    , action : ContextMenuItemActions msg
    }


view : View.Config -> ContextMenuItem msg -> Html msg
view vc (ContextMenuItem { icon, text1, text2, action }) =
    let
        unsetFontStyle =
            [ Css.fontWeight Css.unset, Css.color Css.unset ]
                |> List.map Css.important
                |> css

        ( msg, wrapper ) =
            case action of
                ClickLink link ->
                    ( [], List.singleton >> Html.a [ Html.Styled.Attributes.href link, [ Css.textDecoration Css.none, Css.visited [ Css.color Css.inherit ] ] |> css ] )

                ClickMsg m ->
                    ( [ onClick m ], identity )
    in
    HGraphComponents.rightClickItemStateNeutralTypeWithIconWithAttributes
        (HGraphComponents.rightClickItemStateNeutralTypeWithIconAttributes
            |> Rs.s_stateNeutralTypeWithIcon
                (([ HGraphComponents.rightClickItemStateHoverTypeWithIcon_details.styles
                        ++ HGraphComponents.rightClickItemStateHoverTypeWithIconPlaceholder1_details.styles
                        ++ fullWidth
                        |> Css.hover
                  , Css.cursor Css.pointer
                  ]
                    ++ fullWidth
                    |> css
                 )
                    :: msg
                )
            |> Rs.s_placeholder1
                (unsetFontStyle :: msg)
            |> Rs.s_placeholder2
                (unsetFontStyle :: msg)
            |> Rs.s_iconsDividerNoPadding [ [ Css.position Css.relative ] |> css ]
        )
        { stateNeutralTypeWithIcon =
            { iconInstance = icon
            , text1 = Locale.string vc.locale text1
            , text2 = text2 |> Maybe.withDefault ""
            , text2Visible = Maybe.Extra.isJust text2
            }
        }
        |> wrapper


map : (a -> b) -> ContextMenuItem a -> ContextMenuItem b
map mp (ContextMenuItem { icon, text1, text2, action }) =
    ContextMenuItem
        { icon = Html.map mp icon
        , text1 = text1
        , text2 = text2
        , action =
            case action of
                ClickMsg msg ->
                    ClickMsg (mp msg)

                ClickLink l ->
                    ClickLink l
        }


init :
    { icon : Html msg
    , text : String
    , msg : msg
    }
    -> ContextMenuItem msg
init { icon, text, msg } =
    init2 { icon = icon, text1 = text, text2 = Nothing, msg = msg }


init2 :
    { icon : Html msg
    , text1 : String
    , text2 : Maybe String
    , msg : msg
    }
    -> ContextMenuItem msg
init2 { icon, text1, text2, msg } =
    ContextMenuItem { icon = icon, text1 = text1, text2 = text2, action = ClickMsg msg }


initLink2 :
    { icon : Html msg
    , text1 : String
    , text2 : Maybe String
    , link : String
    }
    -> ContextMenuItem msg
initLink2 { icon, text1, text2, link } =
    ContextMenuItem { icon = icon, text1 = text1, text2 = text2, action = ClickLink link }
