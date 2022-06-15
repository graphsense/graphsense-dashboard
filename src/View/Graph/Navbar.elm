module View.Graph.Navbar exposing (..)

import Config.View exposing (Config)
import Css.Graph as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph exposing (ActiveTool, Model)
import Model.Graph.Tool as Tool
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (Plugins)
import Plugin.Model exposing (PluginStates)
import Plugin.View.Graph.Navbar
import Tuple exposing (..)
import View.Graph.Tool as Tool
import View.Locale as Locale


navbar : Plugins -> PluginStates -> Config -> Model -> Html Msg
navbar plugins states vc model =
    nav
        [ Css.navbar vc |> css
        ]
        [ navbarLeft plugins states vc model
        , navbarRight vc model
        ]


navbarLeft : Plugins -> PluginStates -> Config -> Model -> Html Msg
navbarLeft plugins states vc model =
    div
        [ Css.navbarLeft vc |> css
        ]
        (List.map (Tool.tool vc)
            [ { icon = FontAwesome.icon FontAwesome.tag
              , title = "My tags"
              , msg = ToBeDone
              , color = Nothing
              , status = Tool.Disabled
              }
            ]
            ++ Plugin.View.Graph.Navbar.left plugins states vc model
        )



{-
   entityTag : Html Msg
   entityTag =
       S.svg
           [ S.viewBox "0 0 24 24"
           ]
           [ S.path
               [ S.strokeLineCap "round"
               , S.strokeLineJoin "round"
               , S.strokeWidth "2"
               , S.css
                   [ stroke currentColor
                   , fill none
                   ]
               , S.d "M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z"
               ]
               []
           ]
-}


isActive : (Tool.Toolbox -> Bool) -> ActiveTool -> Tool.Status
isActive is at =
    at.element
        |> Maybe.map
            (\e ->
                if second e && is at.toolbox then
                    Tool.Active

                else
                    Tool.Inactive
            )
        |> Maybe.withDefault Tool.Inactive


navbarRight : Config -> Model -> Html Msg
navbarRight vc model =
    div
        [ Css.navbarRight vc |> css
        ]
        (List.map (Tool.tool vc)
            [ { title = "Start from scratch"
              , icon = FontAwesome.icon FontAwesome.file
              , msg = ToBeDone
              , color = Nothing
              , status = Tool.Disabled
              }
            , { title = "Load from file ..."
              , icon = FontAwesome.icon FontAwesome.folderOpen
              , msg = UserClickedImport
              , color = Nothing
              , status = isActive Tool.isImport model.activeTool
              }
            , { title = "Export ..."
              , icon = FontAwesome.icon FontAwesome.download
              , msg = UserClickedExport
              , color = Nothing
              , status = isActive Tool.isExport model.activeTool
              }
            , { title = "Undo last graph change"
              , icon = FontAwesome.icon FontAwesome.undo
              , msg = ToBeDone
              , color = Nothing
              , status = Tool.Disabled
              }
            , { title = "Redo undone graph change"
              , icon = FontAwesome.icon FontAwesome.redo
              , msg = ToBeDone
              , color = Nothing
              , status = Tool.Disabled
              }
            , { title = "Highlight nodes"
              , icon = FontAwesome.icon FontAwesome.highlighter
              , msg = ToBeDone
              , color = Nothing
              , status = Tool.Disabled
              }
            , { title = "Configuration options"
              , icon = FontAwesome.icon FontAwesome.cog
              , msg = UserClicksConfiguraton
              , color = Nothing
              , status = isActive Tool.isConfiguration model.activeTool
              }
            , { title = "Legend"
              , icon = FontAwesome.icon FontAwesome.info
              , msg = UserClicksLegend
              , color = Nothing
              , status = isActive Tool.isLegend model.activeTool
              }
            ]
        )
