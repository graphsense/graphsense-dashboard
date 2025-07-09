module Update.Pathfinder.WorkflowNextTxByTime exposing (Config, Error(..), Msg, Workflow, start, startBetween, startByHeight, startByTime, update)

import Api.Data
import Api.Request.Addresses exposing (Order_(..))
import Basics.Extra exposing (flip)
import Effect.Api as Api
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as Id exposing (Id)
import RecordSetter exposing (s_value)
import Time
import Workflow


type alias Config =
    { addressId : Id
    , direction : Direction
    }


type Msg
    = BrowserGotBlockHeight Api.Data.BlockAtDate
    | BrowserGotRecentTx Api.Data.AddressTxs
    | BrowserGotTx Api.Data.Tx
    | BrowserGotRecentLink Api.Data.Links


type Error
    = NoTxFound


type alias Workflow =
    Workflow.Workflow Api.Data.Tx Msg Error


startByTime : String -> Time.Posix -> Workflow
startByTime network timestamp =
    BrowserGotBlockHeight
        |> Api.GetBlockByDateEffect
            { currency = network
            , datetime = timestamp
            }
        |> List.singleton
        |> Workflow.Next


startByHeight : Config -> Int -> String -> Workflow
startByHeight config height currency =
    workflowByHeight config (Just height) (Just currency)


start : Config -> Workflow
start config =
    BrowserGotRecentTx
        |> Api.GetAddressTxsEffect
            { currency = Id.network config.addressId
            , address = Id.id config.addressId
            , direction = Just config.direction
            , pagesize = 1
            , nextpage = Nothing
            , order = Just Api.Request.Addresses.Order_Desc
            , tokenCurrency = Nothing
            , minHeight = Nothing
            , maxHeight = Nothing
            }
        |> List.singleton
        |> Workflow.Next


startBetween : Config -> Id -> Workflow
startBetween config neighbor =
    let
        ( source, target ) =
            case config.direction of
                Incoming ->
                    ( neighbor, config.addressId )

                Outgoing ->
                    ( config.addressId, neighbor )
    in
    BrowserGotRecentLink
        |> Api.GetAddresslinkTxsEffect
            { currency = Id.network config.addressId
            , source = Id.id source
            , target = Id.id target
            , pagesize = 1
            , nextpage = Nothing
            , order = Just Api.Request.Addresses.Order_Desc
            , minHeight = Nothing
            , maxHeight = Nothing
            }
        |> List.singleton
        |> Workflow.Next


update : Config -> Msg -> Workflow
update config msg =
    case msg of
        BrowserGotBlockHeight blockAtDate ->
            workflowByHeight config blockAtDate.beforeBlock Nothing

        BrowserGotRecentTx data ->
            data.addressTxs
                |> List.head
                |> Maybe.map
                    (\tx ->
                        case tx of
                            Api.Data.AddressTxAddressTxUtxo t ->
                                BrowserGotTx
                                    |> Api.GetTxEffect
                                        { currency = Id.network config.addressId
                                        , txHash = t.txHash
                                        , includeIo = True
                                        , tokenTxId = Nothing
                                        }
                                    |> List.singleton
                                    |> Workflow.Next

                            Api.Data.AddressTxTxAccount t ->
                                absValues t.value
                                    |> flip s_value t
                                    |> Api.Data.TxTxAccount
                                    |> Workflow.Ok
                    )
                |> Maybe.withDefault (Workflow.Err NoTxFound)

        BrowserGotRecentLink data ->
            data.links
                |> List.head
                |> Maybe.map
                    (\tx ->
                        case tx of
                            Api.Data.LinkLinkUtxo t ->
                                BrowserGotTx
                                    |> Api.GetTxEffect
                                        { currency = Id.network config.addressId
                                        , txHash = t.txHash
                                        , includeIo = True
                                        , tokenTxId = Nothing
                                        }
                                    |> List.singleton
                                    |> Workflow.Next

                            Api.Data.LinkTxAccount t ->
                                absValues t.value
                                    |> flip s_value t
                                    |> Api.Data.TxTxAccount
                                    |> Workflow.Ok
                    )
                |> Maybe.withDefault (Workflow.Err NoTxFound)

        BrowserGotTx tx ->
            Workflow.Ok tx


absValues : Api.Data.Values -> Api.Data.Values
absValues v =
    { v
        | value = abs v.value
        , fiatValues = List.map (\f -> { f | value = abs f.value }) v.fiatValues
    }


workflowByHeight : Config -> Maybe Int -> Maybe String -> Workflow
workflowByHeight config height tokenCurrency =
    BrowserGotRecentTx
        |> Api.GetAddressTxsEffect
            { currency = Id.network config.addressId
            , address = Id.id config.addressId
            , direction = Just config.direction
            , pagesize = 1
            , nextpage = Nothing
            , order =
                Just
                    (case config.direction of
                        Outgoing ->
                            Order_Asc

                        Incoming ->
                            Order_Desc
                    )
            , minHeight =
                case config.direction of
                    Outgoing ->
                        height

                    Incoming ->
                        Nothing
            , maxHeight =
                case config.direction of
                    Outgoing ->
                        Nothing

                    Incoming ->
                        height
            , tokenCurrency = tokenCurrency
            }
        |> List.singleton
        |> Workflow.Next
