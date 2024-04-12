module Update.Pathfinder.Network exposing (..)

import Dict
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (s_data)
import RemoteData exposing (RemoteData(..))
import Result.Extra
import Util.Pathfinder exposing (getAddress)


addressFromRoute : Plugins -> Id -> Network -> ( Network, List Effect )
addressFromRoute plugins id model =
    case Dict.get id model.addresses of
        Nothing ->
            case findAddressPosition id model of
                Ok coords ->
                    coords
                        |> Maybe.Extra.withDefaultLazy (\_ -> findFreePosition model)
                        |> (\c -> loadAddress plugins c id model)

                Err err ->
                    ( model
                    , [ ErrorEffect err ]
                    )

        Just _ ->
            n model


addAddressAt : Plugins -> Id -> Direction -> Id -> Network -> ( Network, List Effect )
addAddressAt plugins at direction id model =
    case getAddress model.addresses at of
        Ok anchor ->
            let
                off =
                    2

                offset =
                    case direction of
                        Incoming ->
                            -off

                        Outgoing ->
                            off
            in
            loadAddress plugins { x = anchor.x + offset, y = anchor.y } id model

        Err err ->
            ( model
            , [ ErrorEffect err ]
            )


updateAddress : Plugins -> Id -> (Address -> Address) -> Network -> Network
updateAddress _ id update model =
    { model | addresses = Dict.update id (Maybe.map update) model.addresses }


loadAddress : Plugins -> Coords -> Id -> Network -> ( Network, List Effect )
loadAddress _ coords id model =
    let
        na =
            coords
                |> Address.init id
                |> s_data Loading
    in
    ( { model
        | addresses = Dict.insert id na model.addresses
      }
    , BrowserGotNewAddress id
        |> Api.GetAddressEffect
            { currency = Id.network id
            , address = Id.id id
            }
        |> ApiEffect
        |> List.singleton
    )


findFreePosition : Network -> Coords
findFreePosition model =
    { x = 0
    , y =
        model.addresses
            |> Dict.values
            |> List.foldl
                (\a y ->
                    max y a.y
                )
                -1
            |> (+) 1
    }


findAddressPosition : Id -> Network -> Result Error (Maybe Coords)
findAddressPosition id network =
    listTxsForAddress network id
        |> List.map (Tx.calcCoords network.addresses)
        |> Result.Extra.combine
        |> Result.map
            (NList.fromList
                >> Maybe.map Coords.avg
            )


expandAddress : Id -> Direction -> Network -> ( Network, List Effect )
expandAddress id direction network =
    ( network
    , BrowserGotRecentTx id direction
        |> Api.GetAddressTxsEffect
            { currency = Id.network id
            , address = Id.id id
            , direction = Just direction
            , pagesize = 1
            , nextpage = Nothing
            }
        |> ApiEffect
        |> List.singleton
    )


insertTx : Tx.Tx -> Network -> Network
insertTx tx network =
    { network
        | txs = Dict.insert tx.id tx network.txs
    }
