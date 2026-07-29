module ReviewConfig exposing (config)

{-| Dead-code analysis. Run it with `make lint-deadcode`, never as part of
`make lint`.

This is deliberately a *second* configuration rather than three more rules in
`review/`, because the `NoUnused` rules below are only correct when every plugin
listed in `config/Config.elm` is checked out.

The reason: `elm.json` is generated, and its `source-directories` gain one entry
per registered plugin. elm-review analyses exactly those directories, so the set
of "used" symbols depends on the checkout. `plugins/` is ignored for *reporting*
(see the bottom of this file), but its files are still read, so usages inside a
plugin do count — as long as the plugin is there.

CI builds with `config/Config.elm.tmp`, which registers no plugins at all. In
that checkout roughly 40 `src/` exports that only plugins use — `View.Button.button`,
`Util.Nullable.toMaybe`, `View.Graph.Table.stringColumn`, … — look unused, and
acting on that report would break all six plugin repositories. `make lint-deadcode`
therefore refuses to run when no plugins are registered.

The longer-term fix is to give plugins an explicit, re-exported API surface
instead of letting them import ~59 arbitrary `src/` modules. Once that exists,
these rules can move into `review/` and run in CI like everything else.

-}

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Exports
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Exports.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    ]
        |> List.map
            (Rule.ignoreErrorsForDirectories
                -- `themes/` is not covered by `make format`, so an automatic fix
                -- there would reformat a hand-maintained file as a side effect.
                [ "generated/", "openapi/", "lib/", "plugins", "src/PluginInterface", "themes/" ]
            )
        |> List.map
            (Rule.ignoreErrorsForFiles
                -- A debugging helper kept on purpose; it is wired in by hand
                -- when someone needs to trace update calls, so it is always
                -- "unused" in a clean tree.
                [ "src/PluginInterface.elm", "src/Util/Debug.elm" ]
            )
