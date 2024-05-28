module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (config)
import Config.UserSettings exposing (default)
import Config.View as View
import Effect exposing (perform)
import Init exposing (init)
import Init.Locale as Locale
import Model exposing (Flags, Model, Msg(..))
import Plugin
import Sub exposing (subscriptions)
import Tuple exposing (..)
import Update exposing (update, updateByUrl)
import Update.Statusbar as Statusbar
import View exposing (view)


plugins : Plugin.Plugins
plugins =
    Config.plugins


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effects ) =
            Statusbar.messagesFromEffects model effects
                |> mapSecond
                    (List.map
                        (\( statusbarToken, eff ) ->
                            perform (Plugin.effectsPlugins plugins) model.key statusbarToken model.user.apiKey eff
                        )
                    )
                |> mapSecond Cmd.batch

        uc =
            { defaultColor = config.theme.graph.defaultColor
            , categoryToColor = config.theme.graph.categoryToColor
            , highlightsColorScheme = config.theme.graph.highlightsColorScheme
            , locale = Locale.init default |> first
            , size = Nothing
            , unit = 0
            }

        updPlug =
            Plugin.updatePlugins plugins

        viewPlugins =
            Plugin.viewPlugins plugins
    in
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, effects ) =
                        init updPlug uc flags url key
                in
                updateByUrl updPlug uc url model
                    |> mapSecond ((++) effects)
                    |> performEffect
        , update =
            \msg model ->
                update updPlug
                    { uc
                        | locale = model.config.locale
                        , size = model.config.size
                        , unit = View.getUnit model.config
                    }
                    msg
                    model
                    |> performEffect
        , view =
            \model ->
                view
                    viewPlugins
                    model.config
                    model
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
