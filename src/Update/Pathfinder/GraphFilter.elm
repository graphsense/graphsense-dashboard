module Update.Pathfinder.GraphFilter exposing (update)

import Model.Pathfinder.GraphFilter as GraphFilter exposing (Model, Period, TimeMode(..))
import Msg.Pathfinder.GraphFilter exposing (Msg(..))
import Set
import Time


{-| `zone` is the one dates are shown in (local time, or UTC per the display
settings): a day picked or typed is that day there.
-}
update : Time.Zone -> Msg -> Model -> Model
update zone msg model =
    case msg of
        UserClickedToggleFilter ->
            -- the popover itself is a toolbar hovercard, see Update.Pathfinder
            model

        UserClickedCloseFilter ->
            { model | editingLimit = False }

        UserClickedChangeLimit ->
            { model | editingLimit = True }

        UserClickedLimitDone ->
            { model | editingLimit = False }

        UserClickedClearAssets ->
            { model | excludedAssets = GraphFilter.init.excludedAssets }

        UserToggledHideTiny ->
            { model | hideTiny = not model.hideTiny }

        UserChangedLimit limit ->
            { model | limit = max 0 limit }

        UserToggledAsset asset ->
            let
                a =
                    String.toLower asset
            in
            { model
                | excludedAssets =
                    if Set.member a model.excludedAssets then
                        Set.remove a model.excludedAssets

                    else
                        Set.insert a model.excludedAssets
            }

        UserChangedPeriod period ->
            { model
                | period = period
                , timeMode =
                    if period == Nothing then
                        AnyTime

                    else
                        model.timeMode
            }

        UserChoseTimeMode mode ->
            case mode of
                AnyTime ->
                    { model | timeMode = AnyTime, period = Nothing }

                OneDay ->
                    { model
                        | timeMode = OneDay
                        , period = model.period |> Maybe.map (.from >> GraphFilter.dayBounds zone)
                    }

                DateRange ->
                    { model | timeMode = DateRange }

        UserChangedDay str ->
            case GraphFilter.fromDateString zone str of
                Just day ->
                    { model | timeMode = OneDay, period = Just (GraphFilter.dayBounds zone day) }

                Nothing ->
                    model

        UserChangedFrom str ->
            GraphFilter.fromDateString zone str
                |> Maybe.map
                    (\from ->
                        { model
                            | timeMode = DateRange
                            , period =
                                Just
                                    (span zone
                                        from
                                        (model.period |> Maybe.map .until |> Maybe.withDefault from)
                                    )
                        }
                    )
                |> Maybe.withDefault model

        UserChangedUntil str ->
            GraphFilter.fromDateString zone str
                |> Maybe.map
                    (\until ->
                        { model
                            | timeMode = DateRange
                            , period =
                                Just
                                    (span zone
                                        (model.period |> Maybe.map .from |> Maybe.withDefault until)
                                        until
                                    )
                        }
                    )
                |> Maybe.withDefault model

        UserPressedStripBucket bucket ->
            { model | stripDrag = Just bucket }

        UserEnteredStripBucket bucket ->
            case model.stripDrag of
                Just anchor ->
                    if anchor == bucket then
                        model

                    else
                        { model | timeMode = DateRange, period = Just (union anchor bucket) }

                Nothing ->
                    model

        UserReleasedStripBucket bucket ->
            case model.stripDrag of
                Just anchor ->
                    if anchor == bucket then
                        -- a click: that day, or that bucket when a bar spans
                        -- several days
                        { model
                            | stripDrag = Nothing
                            , timeMode =
                                if GraphFilter.dayBounds zone bucket.from == bucket then
                                    OneDay

                                else
                                    DateRange
                            , period = Just bucket
                        }

                    else
                        { model | stripDrag = Nothing, timeMode = DateRange, period = Just (union anchor bucket) }

                Nothing ->
                    model

        UserLeftStrip ->
            { model | stripDrag = Nothing }

        UserToggledShowHidden ->
            { model | showHiddenGrey = not model.showHiddenGrey }

        UserClickedReset ->
            -- the limit and the grey display are preferences, not filters:
            -- they survive a reset
            { model
                | hideTiny = GraphFilter.init.hideTiny
                , timeMode = GraphFilter.init.timeMode
                , period = GraphFilter.init.period
                , excludedAssets = GraphFilter.init.excludedAssets
            }


{-| Whole days from the day of `a` to the day of `b`, in either order.
-}
span : Time.Zone -> Time.Posix -> Time.Posix -> Period
span zone a b =
    let
        ( first, last ) =
            if Time.posixToMillis a <= Time.posixToMillis b then
                ( a, b )

            else
                ( b, a )
    in
    { from = (GraphFilter.dayBounds zone first).from
    , until = (GraphFilter.dayBounds zone last).until
    }


union : Period -> Period -> Period
union a b =
    { from = minPosix a.from b.from
    , until = maxPosix a.until b.until
    }


minPosix : Time.Posix -> Time.Posix -> Time.Posix
minPosix a b =
    if Time.posixToMillis a <= Time.posixToMillis b then
        a

    else
        b


maxPosix : Time.Posix -> Time.Posix -> Time.Posix
maxPosix a b =
    if Time.posixToMillis a >= Time.posixToMillis b then
        a

    else
        b
