module PluginApiTest exposing (suite)

{-| Forces `PluginApi` to be compiled.

Nothing in `src/` imports `PluginApi` — it exists only so the dead-code rules
can see which core exports the plugins use. Without this test the module would
never reach the compiler, and a core symbol a plugin depends on could be
deleted or renamed without anything failing until someone built a plugin.

Importing it here means `make test` type-checks the whole plugin contract.
The assertion is incidental; the import is the point.

-}

import Expect
import PluginApi
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "PluginApi"
        [ test "the plugin-facing surface still resolves" <|
            \_ ->
                PluginApi.surface
                    |> List.isEmpty
                    |> Expect.equal False
        ]
