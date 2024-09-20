module Model.Loadable exposing (Loadable(..), id, map, withDefault)


type Loadable id thing
    = Loading String id
    | Loaded thing


map : (a -> b) -> Loadable id a -> Loadable id b
map toValue l =
    case l of
        Loading currency i ->
            Loading currency i

        Loaded a ->
            toValue a |> Loaded


withDefault : a -> Loadable id a -> a
withDefault default l =
    case l of
        Loading _ _ ->
            default

        Loaded v ->
            v


id : (a -> id) -> Loadable id a -> id
id get l =
    case l of
        Loading _ i ->
            i

        Loaded a ->
            get a
