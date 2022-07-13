module View.Graph.Navbar exposing (..)

--import Plugin.View.Graph.Navbar

import Config.View exposing (Config)
import Css.Graph as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph exposing (ActiveTool, History(..), Model)
import Model.Graph.Browser as Browser
import Model.Graph.Tool as Tool
import Msg.Graph exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Tuple exposing (..)
import View.Graph.Tool as Tool
import View.Locale as Locale


navbar : Plugins -> ModelState -> Config -> Model -> Html Msg
navbar plugins states vc model =
    nav
        [ Css.navbar vc |> css
        ]
        [ navbarLeft plugins states vc model
        , navbarRight vc model
        ]


navbarLeft : Plugins -> ModelState -> Config -> Model -> Html Msg
navbarLeft plugins states vc model =
    div
        [ Css.navbarLeft vc |> css
        ]
        (List.map (Tool.tool vc)
            [ { icon = FontAwesome.icon FontAwesome.userTag
              , title = "My tags"
              , msg = \_ -> UserClickedUserTags
              , color = Nothing
              , status =
                    case model.browser.type_ of
                        Browser.UserTags _ ->
                            Tool.Active

                        _ ->
                            Tool.Inactive
              }
            ]
            ++ Plugin.navbarLeft plugins states vc
        )


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
              , msg = \_ -> UserClickedNew
              , color = Nothing
              , status = Tool.Inactive
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
              , msg = \_ -> UserClickedUndo
              , color = Nothing
              , status =
                    case model.history of
                        History past _ ->
                            if List.isEmpty past then
                                Tool.Disabled

                            else
                                Tool.Inactive
              }
            , { title = "Redo undone graph change"
              , icon = FontAwesome.icon FontAwesome.redo
              , msg = \_ -> UserClickedRedo
              , color = Nothing
              , status =
                    case model.history of
                        History _ future ->
                            if List.isEmpty future then
                                Tool.Disabled

                            else
                                Tool.Inactive
              }
            , { title = "Center graph"
              , icon = FontAwesome.icon FontAwesome.compress
              , msg = \_ -> UserClickedFitGraph
              , color = Nothing
              , status = Tool.Inactive
              }
            , { title = "Highlight nodes"
              , icon = FontAwesome.icon FontAwesome.highlighter
              , msg = UserClickedHighlighter
              , color = Nothing
              , status = isActive Tool.isHighlighter model.activeTool
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
