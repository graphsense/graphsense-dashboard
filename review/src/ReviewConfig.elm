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
    -- , NoUnused.CustomTypeConstructors.rule []
    -- , NoUnused.CustomTypeConstructorArgs.rule
    -- , NoUnused.Dependencies.rule
    -- , NoUnused.Exports.rule
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
