module View.Graph.Navbar exposing (..)

import Config.View exposing (Config)
import Css.Graph as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Msg.Graph exposing (Msg(..))
import View.Graph.Tool as Tool
import View.Locale as Locale


navbar : Config -> Html Msg
navbar vc =
    nav
        [ Css.navbar vc |> css
        ]
        [ navbarLeft vc
        , navbarRight vc
        ]


navbarLeft : Config -> Html Msg
navbarLeft vc =
    div
        [ Css.navbarLeft vc |> css
        ]
        (List.map (Tool.tool vc)
            [ { icon = FontAwesome.icon FontAwesome.tag
              , title = Locale.string vc.locale "My tags"
              , msg = NoOp
              }
            ]
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
            [ { title = Locale.string vc.locale "Start from scratch"
              , icon = FontAwesome.icon FontAwesome.file
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Load from file ..."
              , icon = FontAwesome.icon FontAwesome.folderOpen
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Export ..."
              , icon = FontAwesome.icon FontAwesome.download
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Undo last graph change"
              , icon = FontAwesome.icon FontAwesome.undo
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Redo undone graph change"
              , icon = FontAwesome.icon FontAwesome.redo
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Highlight nodes"
              , icon = FontAwesome.icon FontAwesome.highlighter
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Configuration options"
              , icon = FontAwesome.icon FontAwesome.cog
              , msg = NoOp
              }
            , { title = Locale.string vc.locale "Legend"
              , icon = FontAwesome.icon FontAwesome.info
              , msg = NoOp
              }
            ]
        )
