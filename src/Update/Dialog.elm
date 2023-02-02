module Update.Dialog exposing (..)

import Model.Dialog exposing (..)
import Set


confirm : ConfirmConfig msg -> Model msg
confirm =
    Confirm


options : OptionsConfig msg -> Model msg
options =
    Options


addressNotFound : String -> Maybe (Model msg) -> msg -> Model msg
addressNotFound address model onOk =
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
    , onOk = onOk
    }
        |> Error
