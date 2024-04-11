module Init.Pathfinder exposing (..)

import Api.Data
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Search as Search
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (DetailsViewState(..), Model)


dummyAddress : Api.Data.Address
dummyAddress =
    { actors = Nothing
    , address = "bc1qvqxjv6cdf9yxvv5yssujcvt8zu2qfl2nnuuy7d"
    , balance = { value = 100, fiatValues = [ { code = "USD", value = 0.1 }, { code = "EUR", value = 0.2 } ] }
    , currency = "BTC"
    , entity = 1
    , firstTx = { height = 1, timestamp = 1712554025, txHash = "0x0asdfasfasf" }
    , inDegree = 1
    , isContract = Nothing
    , lastTx = { height = 1, timestamp = 1712554025, txHash = "0x0asdfasfasf" }
    , noIncomingTxs = 100
    , noOutgoingTxs = 200
    , outDegree = 1
    , status = Api.Data.AddressStatusClean
    , tokenBalances = Nothing
    , totalReceived = { value = 100, fiatValues = [ { code = "USD", value = 0.1 }, { code = "EUR", value = 0.2 } ] }
    , totalSpent = { value = 100, fiatValues = [ { code = "USD", value = 0.1 }, { code = "EUR", value = 0.2 } ] }
    , totalTokensReceived = Nothing
    , totalTokensSpent = Nothing
    }


init : Model
init =
    { network = Nothing

    --, route = Route.Root
    , selection = []
    , search = Search.init (Search.initSearchAll Nothing)
    , dragging = NoDragging
    , transform = Transform.init
    , history = History.init
    , view = { detailsViewState = Address "bc1qvqxjv6cdf9yxvv5yssujcvt8zu2qfl2nnuuy7d" { addressTableOpen = False, transactionsTableOpen = False } (Just dummyAddress) }
    }
