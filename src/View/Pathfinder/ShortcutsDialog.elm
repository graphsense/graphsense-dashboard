module View.Pathfinder.ShortcutsDialog exposing (view)

{-| The "Keyboard shortcuts" entry behind the help menu: the same list the
hint overlay shows while Ctrl/Cmd is held, for people who never hold the key
long enough to discover it.
-}

import Config.View as View
import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Html.Dialogs as Dialogs
import Util.View exposing (none, pointer, testId)
import View.Locale as Locale
import View.Pathfinder.ShortcutHints as ShortcutHints


view : msg -> View.Config -> Html msg
view closeMsg vc =
    Dialogs.dialogGenericWithAttributes
        (Dialogs.dialogGenericAttributes
            |> Rs.s_root [ testId "gs-shortcuts-dialog" ]
            |> Rs.s_iconsCloseNoPadding [ pointer, onClick closeMsg ]
            |> Rs.s_descriptionFrame [ [ Css.display Css.none ] |> css ]
            |> Rs.s_buttonsLayout [ [ Css.display Css.none ] |> css ]
        )
        { inputList =
            [ div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.property "gap" "6px"
                    ]
                ]
                (ShortcutHints.rows vc)
            ]
        }
        { cancelButton = { variant = none }
        , confirmButton = { variant = none }
        , root = {}
        , dialogHeader =
            { showIconsFrame = False
            , header = Locale.string vc.locale "Keyboard shortcuts"
            , description = ""
            , icon = none
            }
        }
