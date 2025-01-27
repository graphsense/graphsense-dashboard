module View.Pathfinder.ContextMenuItem exposing (ContextMenuItem, init, map, view)

import Config.View as View
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Html.GraphComponents as HGraphComponents
import View.Locale as Locale


type ContextMenuItem msg
    = ContextMenuItem (ContextMenuItemInternal msg)


type alias ContextMenuItemInternal msg =
    { icon : Html msg
    , text : String
    , msg : msg
    }


view : View.Config -> ContextMenuItem msg -> Html msg
view vc (ContextMenuItem { icon, text, msg }) =
    HGraphComponents.rightClickItemStateNeutralTypeWithIconWithAttributes
        (HGraphComponents.rightClickItemStateNeutralTypeWithIconAttributes
            |> Rs.s_stateNeutralTypeWithIcon
                [ [ HGraphComponents.rightClickItemStateHoverTypeWithIcon_details.styles
                        |> Css.hover
                  , Css.cursor Css.pointer
                  ]
                    |> css
                , onClick msg
                ]
            |> Rs.s_placeholder
                [ [ HGraphComponents.rightClickItemStateHoverTypeWithIconPlaceholder_details.styles
                        |> Css.hover
                  ]
                    |> css
                , onClick msg
                ]
        )
        { stateNeutralTypeWithIcon = { iconInstance = icon, text = Locale.string vc.locale text } }


map : (a -> b) -> ContextMenuItem a -> ContextMenuItem b
map mp (ContextMenuItem { icon, text, msg }) =
    ContextMenuItem
        { icon = Html.map mp icon
        , text = text
        , msg = mp msg
        }


init :
    { icon : Html msg
    , text : String
    , msg : msg
    }
    -> ContextMenuItem msg
init =
    ContextMenuItem
