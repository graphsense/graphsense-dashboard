module View.Graph.ExportImport exposing (export, import_)

import Config.View as View
import Css.ContextMenu
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Msg.Graph exposing (Msg(..))
import View.Dialog as Dialog
import View.Locale as Locale


export : View.Config -> List (Html Msg)
export vc =
    [ div
        [ onMouseLeave (UserClickedExport "")
        ]
        [ Dialog.part vc
            "Export"
            [ option vc "GraphSense File (.gs)" (UserClickedExportGS Nothing)
            , option vc "TagPack (.yaml)" (UserClickedExportTagPack Nothing)
            , option vc "Graphics (.svg)" (UserClickedExportGraphics Nothing)
            ]
        ]
    ]


import_ : View.Config -> List (Html Msg)
import_ vc =
    [ div
        [ onMouseLeave (UserClickedImport "")
        ]
        [ Dialog.part vc
            "Import"
            [ option vc "Graphsense File (.gs)" UserClickedImportGS
            , option vc "TagPack (.yaml)" UserClickedImportTagPack
            ]
        ]
    ]


option : View.Config -> String -> msg -> Html msg
option vc title msg =
    div
        [ Css.ContextMenu.option vc |> css
        , onClick msg
        ]
        [ Locale.string vc.locale title
            |> text
        ]
