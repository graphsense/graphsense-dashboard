module Update.Dialog exposing (addressNotFoundError, confirm, generalError, httpError, info, mapMsg, options)

import Html.Styled as Html
import Http
import Model.Dialog exposing (..)
import Set


confirm : ConfirmConfig msg -> Model msg
confirm =
    Confirm


options : OptionsConfig msg -> Model msg
options =
    Options


addressNotFoundError : String -> Maybe (Model msg) -> msg -> Model msg
addressNotFoundError address model onOk =
    { type_ =
        AddressNotFound <|
            case model of
                Just (Error { type_ }) ->
                    case type_ of
                        AddressNotFound addrs ->
                            addrs
                                ++ [ address ]
                                |> Set.fromList
                                |> Set.toList

                        _ ->
                            [ address ]

                _ ->
                    [ address ]
    , onOk = onOk
    }
        |> Error


generalError : GeneralErrorConfig -> msg -> Model msg
generalError conf onOk =
    { type_ = General conf
    , onOk = onOk
    }
        |> Error


httpError : { title : String, error : Http.Error, onOk : msg } -> Model msg
httpError { title, error, onOk } =
    { type_ = Http title error
    , onOk = onOk
    }
        |> Error


info : InfoConfig msg -> Model msg
info =
    Info


mapMsg : (a -> b) -> Model a -> Model b
mapMsg map model =
    case model of
        Confirm conf ->
            { message = conf.message
            , title = conf.title
            , confirmText = conf.confirmText
            , cancelText = conf.cancelText
            , onYes = map conf.onYes
            , onNo = map conf.onNo
            }
                |> Confirm

        Options conf ->
            { message = conf.message
            , options =
                conf.options
                    |> List.map (Tuple.mapSecond map)
            , onClose = map conf.onClose
            }
                |> Options

        Error conf ->
            { type_ = conf.type_
            , onOk = map conf.onOk
            }
                |> Error

        Info conf ->
            { info = conf.info
            , variables = conf.variables
            , title = conf.title
            , onOk = map conf.onOk
            }
                |> Info

        Custom conf ->
            { html = Html.map map conf.html
            , defaultMsg = map conf.defaultMsg
            }
                |> Custom
