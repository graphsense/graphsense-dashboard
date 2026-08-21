module Model.Graph.Browser exposing (Row(..), TableLink, Value(..))

{-| Slim, reusable property-box primitives.

The old `/graph` UI has been removed; this module retains only the generic
property-box data types (`Row`, `Value`, `TableLink`) that are reused as shared
UI primitives (e.g. by plugins). It carries no dependency on the old graph
model (layers, entities, addresses, …). The renderer lives in
`View.Graph.Browser`.

-}

import FontAwesome
import Html.Styled exposing (Html)


type Value msg
    = String String
    | Stack (List (Value msg))
    | Grid Int (List (Value msg))
    | AddressStr String
    | HashStr String
    | Country String String
    | Uri String String
    | IconLink FontAwesome.Icon String
    | InternalLink String String
    | Input (String -> msg) msg String
    | Select (List ( String, String )) (String -> msg) String
    | Html (Html msg)


type alias TableLink =
    { title : String
    , link : String
    , active : Bool
    }


type Row r i msg
    = Row ( String, r, Maybe TableLink )
    | RowWithMoreActionsButton ( String, r, Maybe (i -> msg) )
    | Note String
    | Footnote String
    | Image (Maybe String)
    | Rule
    | OptionalRow (Row r i msg) Bool
