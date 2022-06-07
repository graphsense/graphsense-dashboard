module View.Graph.Navbar exposing (..)

import Config.View exposing (Config)
import Css.Graph as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph exposing (Model)
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (Plugins)
import Plugin.Model exposing (PluginStates)
import Plugin.View.Graph.Navbar
import View.Graph.Tool as Tool
import View.Locale as Locale


navbar : Plugins -> PluginStates -> Config -> Model -> Html Msg
navbar plugins states vc model =
    nav
        [ Css.navbar vc |> css
        ]
        [ navbarLeft plugins states vc model
        , navbarRight vc
        ]


navbarLeft : Plugins -> PluginStates -> Config -> Model -> Html Msg
navbarLeft plugins states vc model =
    div
        [ Css.navbarLeft vc |> css
        ]
        (List.map (Tool.tool vc)
            [ { icon = FontAwesome.icon FontAwesome.tag
              , title = "My tags"
              , msg = ToBeDone "My tags"
              , color = Nothing
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


navbarRight : Config -> Html Msg
navbarRight vc =
    div
        [ Css.navbarRight vc |> css
        ]
        (List.map (Tool.tool vc)
            [ { title = "Start from scratch"
              , icon = FontAwesome.icon FontAwesome.file
              , msg = ToBeDone "Start from scratch"
              , color = Nothing
              }
            , { title = "Load from file ..."
              , icon = FontAwesome.icon FontAwesome.folderOpen
              , msg = ToBeDone "Load from file ..."
              , color = Nothing
              }
            , { title = "Export ..."
              , icon = FontAwesome.icon FontAwesome.download
              , msg = ToBeDone "Export ..."
              , color = Nothing
              }
            , { title = "Undo last graph change"
              , icon = FontAwesome.icon FontAwesome.undo
              , msg = ToBeDone "Undo last graph change"
              , color = Nothing
              }
            , { title = "Redo undone graph change"
              , icon = FontAwesome.icon FontAwesome.redo
              , msg = ToBeDone "Redo undone graph change"
              , color = Nothing
              }
            , { title = "Highlight nodes"
              , icon = FontAwesome.icon FontAwesome.highlighter
              , msg = ToBeDone "Highlight nodes"
              , color = Nothing
              }
            , { title = "Configuration options"
              , icon = FontAwesome.icon FontAwesome.cog
              , msg = ToBeDone "Configuration options"
              , color = Nothing
              }
            , { title = "Legend"
              , icon = FontAwesome.icon FontAwesome.info
              , msg = ToBeDone "Legend"
              , color = Nothing
              }
            ]
        )
