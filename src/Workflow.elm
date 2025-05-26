module Workflow exposing (Workflow(..), mapEffect, next)

import Effect.Api as Api


type Workflow ok msg err
    = Ok ok
    | Next (List (Api.Effect msg))
    | Err err


mapEffect : (a -> b) -> Workflow ok a err -> Workflow ok b err
mapEffect map wf =
    case wf of
        Next eff ->
            List.map (Api.map map) eff
                |> Next

        Ok x ->
            Ok x

        Err x ->
            Err x


next : Workflow ok msg err -> List (Api.Effect msg)
next wf =
    case wf of
        Next eff ->
            eff

        _ ->
            []
