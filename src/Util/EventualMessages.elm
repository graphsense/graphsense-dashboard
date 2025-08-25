module Util.EventualMessages exposing (EventualMessages, addMessage, dispatchMessages, heartBeat, init, setMaxAge, setMillisecondsPerEpoch)

import Process
import Task


type EventualMessages c m o
    = Internal (EventualMessagesI c m o)


type alias Condition c o =
    { condition : c
    , message : o
    , age : Int
    }


type alias EventualMessagesI c m o =
    { msgs : List (Condition c o)
    , conditionChecker : c -> m -> Bool
    , maxAge : Int
    , millisecondsPerEpoch : Int
    , heartBeatOutput : o
    }


init : (c -> m -> Bool) -> o -> EventualMessages c m o
init conditionChecker heartBeatOutput =
    { msgs = []
    , conditionChecker = conditionChecker
    , maxAge = 5
    , millisecondsPerEpoch = 1000
    , heartBeatOutput = heartBeatOutput
    }
        |> Internal


setMillisecondsPerEpoch : Int -> EventualMessages c m o -> EventualMessages c m o
setMillisecondsPerEpoch newMillisecondsPerEpoch (Internal model) =
    { model | millisecondsPerEpoch = newMillisecondsPerEpoch } |> Internal


setMaxAge : Int -> EventualMessages c m o -> EventualMessages c m o
setMaxAge newMaxAge (Internal model) =
    { model | maxAge = newMaxAge } |> Internal


addMessage : c -> o -> EventualMessages c m o -> ( EventualMessages c m o, Maybe (Cmd o) )
addMessage condition message (Internal model) =
    let
        newCondition =
            { condition = condition, message = message, age = 0 }

        newModel =
            { model | msgs = newCondition :: model.msgs }

        -- Start heartbeat if this is the first message
        wasEmpty =
            List.isEmpty model.msgs

        heartBeatCmd =
            if wasEmpty then
                -- Start the heartbeat since we went from empty to having messages
                Process.sleep (toFloat model.millisecondsPerEpoch)
                    |> Task.perform (\_ -> model.heartBeatOutput)
                    |> Just

            else
                Nothing
    in
    ( Internal newModel, heartBeatCmd )


heartBeat : EventualMessages c m o -> ( EventualMessages c m o, Maybe (Cmd o) )
heartBeat (Internal model) =
    let
        newModel =
            { model
                | msgs = List.filterMap (updateConditionAge model) model.msgs
            }

        -- Create delayed task for next heartbeat
        delayCmd =
            Process.sleep (toFloat model.millisecondsPerEpoch)
                |> Task.perform (\_ -> model.heartBeatOutput)
    in
    if newModel.msgs |> List.isEmpty then
        -- No messages left, return the model without commands
        ( Internal newModel, Nothing )

    else
        ( Internal newModel, Just delayCmd )


dispatchMessages : m -> EventualMessages c m o -> ( EventualMessages c m o, Maybe (Cmd o) )
dispatchMessages currentModel (Internal model) =
    let
        -- Check each condition and collect fulfilled messages
        ( remainingMsgs, fulfilledMsgs ) =
            List.foldl (checkCondition model currentModel) ( [], [] ) model.msgs

        newModel =
            { model | msgs = List.reverse remainingMsgs }

        -- Convert fulfilled messages to commands
        fulfilledCmds =
            fulfilledMsgs
                |> List.map (\msg -> Task.succeed msg |> Task.perform identity)
                |> Cmd.batch
    in
    if fulfilledMsgs |> List.isEmpty then
        -- No fulfilled messages, return the model without commands
        ( Internal newModel, Nothing )

    else
        -- Return the updated model and the commands for fulfilled messages
        ( Internal newModel, Just fulfilledCmds )


checkCondition : EventualMessagesI c m o -> m -> Condition c o -> ( List (Condition c o), List o ) -> ( List (Condition c o), List o )
checkCondition model currentModel condition ( remainingMsgs, fulfilledMsgs ) =
    if model.conditionChecker condition.condition currentModel then
        -- Condition fulfilled, add message to fulfilled list, don't add to remaining
        ( remainingMsgs, condition.message :: fulfilledMsgs )

    else
        -- Condition not fulfilled, keep in remaining list
        ( condition :: remainingMsgs, fulfilledMsgs )


updateConditionAge : EventualMessagesI c m o -> Condition c o -> Maybe (Condition c o)
updateConditionAge model condition =
    let
        newAge =
            condition.age + 1

        isExpired =
            newAge > model.maxAge

        newCondition =
            { condition | age = newAge }
    in
    if isExpired then
        Nothing

    else
        Just newCondition
