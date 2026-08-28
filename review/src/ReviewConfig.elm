module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.ReviewAtDocs
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoRedundantlyQualifiedType
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


{-| Exemptions for the three dead-code rules.

These rules are only correct because `src/PluginApi.elm` references every core
symbol the plugins use: `elm.json` is generated and only lists the plugins that
happen to be checked out, and CI has none, so without that module ~40 core
exports would look dead in CI. `PluginApi` itself is therefore exempt -- its
only consumers live in six other repositories.

`Util/Debug.elm` is a hand-wired debugging helper, always "unused" in a clean
tree. `Util/Nullable.elm` is used only by a plugin *and* imports a module from a
plugin's generated api directory, so `PluginApi` cannot reference it without
breaking the build wherever that plugin is absent -- it should really move into
that plugin. `themes/` is not covered by `make format`, so an automatic fix there
would reformat a hand-maintained file as a side effect.

-}
ignoreForDeadCode : Rule -> Rule
ignoreForDeadCode =
    Rule.ignoreErrorsForFiles [ "src/PluginApi.elm", "src/Util/Debug.elm", "src/Util/Nullable.elm", "src/Log.elm" ]
        >> Rule.ignoreErrorsForDirectories [ "themes/", "src/Components" ]


config : List Rule
config =
    [ --   Docs.ReviewAtDocs.rule
      -- NoConfusingPrefixOperator.rule
      NoDebug.Log.rule |> Rule.ignoreErrorsForFiles [ "src/Util/Debug.elm" ]
    , NoDebug.TodoOrToString.rule |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule

    -- , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule

    -- , NoUnnecessaryTrailingUnderscore.rule
    , NoRedundantlyQualifiedType.rule

    -- , NoPrematureLetComputation.rule
    -- , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule |> ignoreForDeadCode
    , NoUnused.CustomTypeConstructors.rule [] |> ignoreForDeadCode
    , NoUnused.CustomTypeConstructorArgs.rule |> ignoreForDeadCode
    , NoUnused.Parameters.rule

    -- , NoUnused.Patterns.rule
    , Simplify.rule Simplify.defaults

    -- NoUnused.CustomTypeConstructors.rule []
    -- NoUnused.CustomTypeConstructorArgs.rule
    -- , NoUnused.Dependencies.rule
    -- , NoUnused.Exports.rule |> Rule.ignoreErrorsForFiles ["src/View/Locale.elm"]
    -- , NoUnused.Parameters.rule
    -- , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]
    |> List.map (Rule.ignoreErrorsForDirectories [ "generated/", "openapi/", "lib/", "plugins", "src/PluginInterface"])
    |> List.map (Rule.ignoreErrorsForFiles [ "src/PluginInterface.elm" ])
