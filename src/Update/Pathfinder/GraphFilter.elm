module Update.Pathfinder.GraphFilter exposing (update)

import Model.Pathfinder.GraphFilter as GraphFilter exposing (Model)
import Msg.Pathfinder.GraphFilter exposing (Msg(..))
import Set


update : Msg -> Model -> Model
update msg model =
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
            { model | period = period }

        UserToggledShowHidden ->
            { model | showHiddenGrey = not model.showHiddenGrey }

        UserClickedReset ->
            -- the limit and the grey display are preferences, not filters:
            -- they survive a reset
            { model
                | hideTiny = GraphFilter.init.hideTiny
                , period = GraphFilter.init.period
                , excludedAssets = GraphFilter.init.excludedAssets
            }
