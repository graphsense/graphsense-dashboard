module Update.Graph exposing (..)

import Api.Data
import Config.Update as Update
import Effect exposing (n)
import Effect.Graph exposing (Effect(..))
import Model.Graph exposing (..)
import Msg.Graph as Msg exposing (Msg(..))
import Route
import Set exposing (Set)
import Update.Graph.Adding as Adding
import Update.Graph.Color as Color
import Update.Graph.Layer as Layer


addAddressAndEntity : Update.Config -> Api.Data.Address -> Api.Data.Entity -> Model -> ( Model, List Effect )
addAddressAndEntity uc address entity model =
    let
        addedEntity =
            Layer.addEntity uc model.colors entity model.layers

        added =
            Layer.addAddress uc addedEntity.colors address addedEntity.layers

        adding =
            Adding.checkAddress { currency = address.currency, address = address.address } model.adding
                |> Adding.checkEntity { currency = entity.currency, entity = entity.entity }
    in
    { model
        | adding = adding
        , layers = added.layers
        , colors = added.colors
    }
        |> n


addAddress : Update.Config -> Api.Data.Address -> Model -> ( Model, List Effect )
addAddress uc address model =
    let
        added =
            Layer.addAddress uc model.colors address model.layers
    in
    { model
        | adding = Adding.checkAddress { currency = address.currency, address = address.address } model.adding
        , layers = added.layers
        , colors = added.colors
    }
        |> n


addEntity : Update.Config -> Api.Data.Entity -> Model -> ( Model, List Effect )
addEntity uc entity model =
    let
        added =
            Layer.addEntity uc model.colors entity model.layers
    in
    { model
        | adding = Adding.checkEntity { currency = entity.currency, entity = entity.entity } model.adding
        , layers = added.layers
        , colors = added.colors
    }
        |> n


update : Msg -> Model -> ( Model, List Effect )
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

        UserClickedEntity id ->
            n model

        UserRightClickedEntity id ->
            n model

        UserHoversEntity id ->
            n model

        UserLeavesEntity id ->
            n model

        UserClickedEntityExpandHandle id isOutgoing ->
            n model

        UserClickedAddressExpandHandle id isOutgoing ->
            n model

        NoOp ->
            n model


addingAddress : { currency : String, address : String } -> Model -> ( Model, List Effect )
addingAddress { currency, address } model =
    { model
        | adding = Adding.addAddress { currency = currency, address = address } model.adding
    }
        |> n


addingEntity : { currency : String, entity : Int } -> Model -> ( Model, List Effect )
addingEntity { currency, entity } model =
    { model
        | adding = Adding.addEntity { currency = currency, entity = entity } model.adding
    }
        |> n


addingLabel : String -> Model -> ( Model, List Effect )
addingLabel label model =
    { model
        | adding = Adding.addLabel label model.adding
    }
        |> n
