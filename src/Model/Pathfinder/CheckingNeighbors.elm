module Model.Pathfinder.CheckingNeighbors exposing (Model, done, getData, init, initAddress, insert, isEmpty, member, remove, removeAll)

import Api.Data
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id exposing (Id)
import RecordSetter exposing (s_incoming, s_outgoing)
import Set exposing (Set)


type Model
    = CheckingNeighbors (Dict Id ModelInternal)


type alias ModelInternal =
    { data : Api.Data.Address
    , outRequestIds : List Id
    , inRequestIds : List Id
    , outgoing : Maybe (Set Id)
    , incoming : Maybe (Set Id)
    }


init : Model
init =
    CheckingNeighbors Dict.empty


initAddress : Api.Data.Address -> List Id -> List Id -> Model -> Model
initAddress data outRequestIds inRequestIds (CheckingNeighbors model) =
    let
        id =
            Id.init data.currency data.address
    in
    { data = data
    , outRequestIds = outRequestIds
    , inRequestIds = inRequestIds
    , outgoing = Nothing
    , incoming = Nothing
    }
        |> flip (Dict.insert id) model
        |> CheckingNeighbors


member : Id -> Model -> Bool
member addressId (CheckingNeighbors cn) =
    Dict.member addressId cn


insert : Direction -> Id -> List Api.Data.NeighborAddress -> Model -> Model
insert direction addressId neighbors (CheckingNeighbors model) =
    Dict.update addressId
        (Maybe.map
            (\cn ->
                let
                    nn =
                        neighbors
                            |> List.map (.address >> Id.initFromRecord)
                            |> Set.fromList
                            |> Just
                in
                { cn
                    | outgoing =
                        if direction == Outgoing then
                            nn

                        else
                            cn.outgoing
                    , incoming =
                        if direction == Incoming then
                            nn

                        else
                            cn.incoming
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
    case ( cn.incoming, cn.outgoing ) of
        ( Just a, Just b ) ->
            Set.isEmpty a && Set.isEmpty b

        _ ->
            False


done_ : ModelInternal -> Bool
done_ cn =
    cn.incoming /= Nothing && cn.outgoing /= Nothing


remove : Id -> Id -> Model -> Model
remove addressId neighborId (CheckingNeighbors model) =
    Dict.update addressId
        (Maybe.andThen
            (\cn ->
                let
                    newIncoming =
                        Maybe.map (Set.remove neighborId) cn.incoming

                    newOutgoing =
                        Maybe.map (Set.remove neighborId) cn.outgoing
                in
                if newIncoming == Just Set.empty && newOutgoing == Just Set.empty then
                    -- leads to removal of the addressId from the dict
                    Nothing

                else
                    s_incoming newIncoming cn
                        |> s_outgoing newOutgoing
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


removeAll : Id -> Model -> Model
removeAll id (CheckingNeighbors model) =
    Dict.get id model
        |> Maybe.map
            (s_incoming (Just Set.empty)
                >> s_outgoing (Just Set.empty)
                >> flip (Dict.insert id) model
            )
        |> Maybe.withDefault model
        |> CheckingNeighbors


done : Id -> Model -> Bool
done addressId (CheckingNeighbors model) =
    Dict.get addressId model
        |> Maybe.map done_
        |> Maybe.withDefault True
