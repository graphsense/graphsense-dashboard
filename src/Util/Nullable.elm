module Util.Nullable exposing (fromMaybe, toMaybe)

import OpenApi.Common exposing (Nullable(..))


fromMaybe : Maybe a -> Nullable a
fromMaybe =
    Maybe.map Present
        >> Maybe.withDefault Null


toMaybe : Nullable a -> Maybe a
toMaybe a =
    case a of
        Null ->
            Nothing

        Present aa ->
            Just aa
