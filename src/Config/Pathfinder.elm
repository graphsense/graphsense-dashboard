module Config.Pathfinder exposing (Config, HideForExport(..), TracingMode(..), addressRadius, autoLinkContractAddresses, bulkFetchSizeForExportSize, nodeXOffset, nodeYOffset, numberOfRowsForCSVExport)

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


type alias Config =
    { snapToGrid : Bool
    , highlightClusterFriends : Bool
    , tracingMode : TracingMode
    , avoidOverlapingNodes : Bool
    , hideForExport : HideForExport
    }
