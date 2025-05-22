module Model.Pathfinder.CheckingNeighbors exposing (Model, getData, init, initAddress, insert, isEmpty, member, remove)

import Api.Data
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id exposing (Id)
import RecordSetter as Rs
import Set exposing (Set)


type Model
    = CheckingNeighbors (Dict Id ModelInternal)


type alias ModelInternal =
    { data : Api.Data.Address
    , gotOutgoing : Bool
    , gotIncoming : Bool
    , neighbors : Set Id
    }


init : Model
init =
    CheckingNeighbors Dict.empty


initAddress : Api.Data.Address -> Model -> Model
initAddress data (CheckingNeighbors model) =
    let
        id =
            Id.init data.currency data.address
    in
    { data = data
    , gotOutgoing = False
    , gotIncoming = False
    , neighbors = Set.empty
    }
        |> flip (Dict.insert id) model
        |> CheckingNeighbors


member : Id -> Model -> Bool
member addressId (CheckingNeighbors cn) =
    Dict.member addressId cn


insert : Direction -> Id -> List Id -> Model -> Model
insert direction addressId neighborIds (CheckingNeighbors model) =
    Dict.update addressId
        (Maybe.map
            (\cn ->
                { cn
                    | neighbors =
                        neighborIds
                            |> List.foldl Set.insert cn.neighbors
                    , gotOutgoing = cn.gotOutgoing || direction == Outgoing
                    , gotIncoming = cn.gotIncoming || direction == Incoming
                }
            )
            >> Maybe.andThen
                (\cn ->
                    if isEmpty_ cn then
                        Nothing

                    else
                        Just cn
                )
        )
        model
        |> CheckingNeighbors


isEmpty_ : ModelInternal -> Bool
isEmpty_ cn =
    cn.gotIncoming && cn.gotOutgoing && Set.isEmpty cn.neighbors


remove : Id -> Id -> Model -> Model
remove addressId neighborId (CheckingNeighbors model) =
    Dict.update addressId
        (Maybe.andThen
            (\cn ->
                let
                    newSet =
                        Set.remove neighborId cn.neighbors
                in
                if Set.isEmpty newSet then
                    -- leads to removal of the addressId from the dict
                    Nothing

                else
                    Rs.s_neighbors newSet cn
                        |> Just
            )
        )
        model
        |> CheckingNeighbors


isEmpty : Id -> Model -> Bool
isEmpty addressId (CheckingNeighbors model) =
    Dict.get addressId model
        |> Maybe.map isEmpty_
        |> Maybe.withDefault True


getData : Id -> Model -> Maybe Api.Data.Address
getData id (CheckingNeighbors model) =
    Dict.get id model |> Maybe.map .data
