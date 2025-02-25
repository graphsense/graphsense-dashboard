module Model.Pathfinder.Table.RelatedAddressesTable exposing (Model)

import Api.Data
import Api.Request.Addresses
import Model.DateRangePicker as DateRangePicker
import Model.Graph.Table as Table
import Model.Pathfinder.PagedTable exposing (PagedTable)
import Msg.Pathfinder.AddressDetails exposing (Msg)


type alias Model =
    {}


titleAddress : String
titleAddress =
    "Address"


titleValue : String
titleValue =
    "Value"
