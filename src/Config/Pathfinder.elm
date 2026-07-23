module Config.Pathfinder exposing (AggEdgeFilter(..), Config, HideForExport(..), TracingMode(..), addressRadius, autoLinkContractAddresses, bulkFetchSizeForExportSize, nodeXOffset, nodeYOffset, numberOfRowsForCSVExport)

{-| When auto-linking newly added address nodes to their visible neighbors,
contract calls (links where either endpoint is a smart contract) are omitted by
default to keep traces clean. Set this to `True` to restore the old behavior and
auto-link contract addresses as well.
-}


autoLinkContractAddresses : Bool
autoLinkContractAddresses =
    False


addressRadius : Float
addressRadius =
    1


nodeXOffset : Float
nodeXOffset =
    4


nodeYOffset : Float
nodeYOffset =
    2.5


numberOfRowsForCSVExport : Int
numberOfRowsForCSVExport =
    5000


bulkFetchSizeForExportSize : Int
bulkFetchSizeForExportSize =
    100


type TracingMode
    = TransactionTracingMode
    | AggregateTracingMode


type HideForExport
    = NoExport
    | Exporting Bool


{-| In aggregate mode, which subset of agg-edges to show.

  - `AllAggEdges` — every agg-edge (default)
  - `OnlyTxBacked` — only edges with at least one underlying tx on the canvas
  - `OnlyNew` — only edges whose underlying-tx set is empty (i.e. revealed
    via aggregate exploration without any tx yet traced through them)

-}
type AggEdgeFilter
    = AllAggEdges
    | OnlyTxBacked
    | OnlyNew


type alias Config =
    { snapToGrid : Bool
    , highlightClusterFriends : Bool
    , tracingMode : TracingMode
    , avoidOverlapingNodes : Bool
    , hideForExport : HideForExport
    , aggEdgeFilter : AggEdgeFilter
    }
