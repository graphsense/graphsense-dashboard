module Graph.Update exposing (..)

import Api.Data
import Graph.Effect exposing (Effect(..), n)
import Graph.Model exposing (..)
import Graph.Msg as Msg exposing (Msg(..))
import Graph.Update.Adding as Adding
import Graph.Update.Layer as Layer
import Route
import Set exposing (Set)


addAddress : Api.Data.Address -> Model -> ( Model, Effect )
addAddress address model =
    case Adding.checkAddress { currency = address.currency, address = address.address } model.adding of
        Nothing ->
            n model

        Just addi ->
            let
                added =
                    Layer.addAddress address model.layers
            in
            { model
                | adding = addi
                , layers = added.layers
            }
                |> n


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        UserClickedAddress id ->
            n model

        UserRightClickedAddress id ->
            n model

        UserHoversAddress id ->
            n model

        UserLeavesAddress id ->
            n model

        NoOp ->
            n model


addingAddress : { currency : String, address : String } -> Model -> ( Model, Effect )
addingAddress { currency, address } model =
    { model
        | adding = Adding.addAddress { currency = currency, address = address } model.adding
    }
        |> n


addingLabel : String -> Model -> ( Model, Effect )
addingLabel label model =
    { model
        | adding = Adding.addLabel label model.adding
    }
        |> n
