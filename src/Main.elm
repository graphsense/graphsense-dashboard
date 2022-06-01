module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (config)
import Dict
import Effect exposing (perform)
import Init exposing (init)
import Model exposing (Flags, Model, Msg(..))
import Model.Locale as Locale
import Sub exposing (subscriptions)
import Tuple exposing (..)
import Update exposing (update, updateByUrl)
import View exposing (view)
import View.Locale as Locale


plugins =
    Dict.fromList
        Config.plugins


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effects ) =
            ( model, List.map (perform plugins model.key model.user.apiKey) effects |> Cmd.batch )

        uc =
            { defaultColor = config.theme.graph.defaultColor
            , colorScheme = config.theme.graph.colorScheme
            }
    in
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, effects ) =
                        init plugins flags url key
                in
                updateByUrl plugins uc url model
                    |> mapSecond ((++) effects)
                    |> performEffect
        , update =
            \msg model ->
                update
                    plugins
                    uc
                    msg
                    model
                    |> performEffect
        , view =
            \model ->
                view
                    plugins
                    model.config
                    model
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
