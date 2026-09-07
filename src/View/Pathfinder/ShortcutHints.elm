module View.Pathfinder.ShortcutHints exposing (view)

{-| The overlay that lists the keyboard shortcuts while Ctrl/Cmd is held down,
the way command-line tools show their key bindings. Purely informational: it
takes no pointer events and closes with the key.
-}

import Config.View as View
import Css
import Css.Pathfinder exposing (mGap)
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (css)
import Theme.Colors as Colors
import Util.Pathfinder.Shortcuts as Shortcuts exposing (Shortcut)
import Util.View exposing (testId)
import View.Locale as Locale


view : View.Config -> Html msg
view vc =
    div
        [ testId "gs-shortcut-hints"
        , css
            [ Css.position Css.absolute
            , Css.left mGap
            , Css.bottom mGap
            , Css.zIndex (Css.int 100)
            , Css.pointerEvents Css.none
            , Css.padding2 (Css.px 10) (Css.px 14)
            , Css.borderRadius (Css.px 8)
            , Css.property "background-color" Colors.white
            , Css.property "border" ("1px solid " ++ Colors.greyBlue100)
            , Css.property "box-shadow" ("0 2px 8px " ++ Colors.greyShadow)
            , Css.fontFamilies [ "Roboto" ]
            , Css.fontSize (Css.px 12)
            , Css.property "color" Colors.brandText
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "6px"
            ]
        ]
        (div
            [ css
                [ Css.fontWeight (Css.int 500)
                , Css.marginBottom (Css.px 2)
                ]
            ]
            [ Locale.string vc.locale "Keyboard shortcuts" |> text ]
            :: List.map (row vc) Shortcuts.all
        )


row : View.Config -> Shortcut -> Html msg
row vc shortcut =
    let
        modKey =
            if shortcut.withMod then
                [ key (Shortcuts.modKeyLabel vc.isMac) ]

            else
                []
    in
    div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "4px"
            ]
        ]
        (modKey
            ++ List.map key shortcut.keys
            ++ [ span
                    [ css [ Css.marginLeft (Css.px 6) ] ]
                    [ Locale.string vc.locale shortcut.label |> text ]
               ]
        )


key : String -> Html msg
key label =
    span
        [ css
            [ Css.display Css.inlineBlock
            , Css.minWidth (Css.px 14)
            , Css.padding2 (Css.px 1) (Css.px 5)
            , Css.borderRadius (Css.px 4)
            , Css.property "border" ("1px solid " ++ Colors.greyBlue200)
            , Css.property "background-color" Colors.greyBlue20
            , Css.textAlign Css.center
            , Css.fontFamilies [ "monospace" ]
            , Css.fontSize (Css.px 11)
            ]
        ]
        [ text label ]
