module Util.BridgeExplorer exposing (ExplorerLink, appName, explorerLink, legTxHash, protocolName)

{-| Which bridge a conversion went through, named as people know it, and where
the protocol's own explorer shows the transfer.

The API names the protocol of a bridge row (`protocol`, `protocol_app`,
`transfer_id`; adapter DASHBOARD\_CHANGES D-42). The links are built here,
like the chain explorers in `Util.ExternalLinks`. Each opens only when clicked,
and that click tells the explorer's operator which transfer was looked at.

A hash or id that is not plain hex never becomes a URL.

-}


type alias ExplorerLink =
    { url : String
    , label : String
    }


protocolName : String -> String
protocolName protocol =
    case protocol of
        "cctp" ->
            "CCTP"

        "debridge" ->
            "deBridge"

        "layerzero" ->
            "LayerZero"

        "thorchain" ->
            "THORChain"

        "across" ->
            "Across"

        "relay" ->
            "Relay"

        "wormhole" ->
            "Wormhole"

        "symbiosis" ->
            "Symbiosis"

        other ->
            other


appName : String -> String
appName app =
    case app of
        "stargate" ->
            "Stargate"

        "usdt0" ->
            "USDT0"

        other ->
            other


{-| The tx hash of a leg identifier (`<hash>_T<n>`, `<hash>_I<n>` or a bare
hash), without a `0x` prefix.
-}
legTxHash : String -> String
legTxHash identifier =
    identifier
        |> String.split "_"
        |> List.head
        |> Maybe.withDefault identifier
        |> dropHexPrefix


{-| The protocol's explorer page for a transfer: LayerZero Scan finds a message
by its source tx, THORChain Explorer (thorchain.net) a swap by its inbound tx
(the source leg), deExplorer an order by its id. Each was opened on a real
transfer on 2026-11-06.

Circle runs no CCTP explorer. Range's (usdc.range.org, a third party listed
in Circle's partner directory) searches a tx hash, which must start with 0x,
and opens the transfer with both legs, or lists the matches when there are
several. Its `status?id=` pages take an undocumented id, so we link the search.
Checked on 2026-11-09 on V1 and V2 transfers from all seven CCTP networks we
serve; V1 transfers from 2023/24 were not found.

Across opens a transfer by its origin tx (`across.to/transfer/0x<hash>`; the
fill tx is not found there), Relay a request by its deposit tx
(`relay.link/transaction/0x<hash>`, Relay's documented link; its page sits
behind a bot check, so only a browser opens it), Wormholescan a message by its
VAA id from either end. A half row without its source leg links no Across or
Relay page. Checked on 2026-11-17 on the adapter's recorded transfers.

-}
explorerLink : { protocol : String, sourceLeg : String, transferId : Maybe String } -> Maybe ExplorerLink
explorerLink { protocol, sourceLeg, transferId } =
    let
        source =
            legTxHash sourceLeg
                |> hexOnly
    in
    case protocol of
        "layerzero" ->
            source
                |> Maybe.map
                    (\hash ->
                        { url = "https://layerzeroscan.com/tx/0x" ++ String.toLower hash
                        , label = "LayerZero Scan"
                        }
                    )

        "thorchain" ->
            source
                |> Maybe.map
                    (\hash ->
                        { url = "https://thorchain.net/tx/" ++ String.toUpper hash
                        , label = "THORChain Explorer"
                        }
                    )

        "debridge" ->
            transferId
                |> Maybe.andThen orderId
                |> Maybe.map
                    (\id ->
                        { url = "https://app.debridge.com/order?orderId=" ++ id
                        , label = "deExplorer"
                        }
                    )

        "cctp" ->
            source
                |> Maybe.map
                    (\hash ->
                        { url = "https://usdc.range.org/transactions?s=0x" ++ String.toLower hash
                        , label = "Range CCTP Explorer"
                        }
                    )

        "across" ->
            source
                |> Maybe.andThen txHash
                |> Maybe.map
                    (\hash ->
                        { url = "https://across.to/transfer/0x" ++ hash
                        , label = "Across Explorer"
                        }
                    )

        "relay" ->
            source
                |> Maybe.andThen txHash
                |> Maybe.map
                    (\hash ->
                        { url = "https://relay.link/transaction/0x" ++ hash
                        , label = "Relay Explorer"
                        }
                    )

        "wormhole" ->
            transferId
                |> Maybe.andThen vaaId
                |> Maybe.map
                    (\id ->
                        { url = "https://wormholescan.io/#/tx/" ++ id
                        , label = "Wormholescan"
                        }
                    )

        _ ->
            Nothing


{-| A tx hash: 32 bytes of hex (no 0x), lowercased.
-}
txHash : String -> Maybe String
txHash hash =
    if String.length hash == 64 then
        hexOnly hash |> Maybe.map String.toLower

    else
        Nothing


{-| A Wormhole VAA id as Wormholescan takes it, `<chain>/<emitter>/<sequence>`:
the Wormhole chain id (its own numbering: eth 2, bnb 4), the emitter as 64 hex
digits without 0x (its leading zeros are part of it) and the sequence, both in
decimal.
-}
vaaId : String -> Maybe String
vaaId id =
    case String.split "/" id of
        [ chain, emitter, sequence ] ->
            if
                decimal chain
                    && String.length emitter
                    == 64
                    && hexOnly emitter
                    /= Nothing
                    && decimal sequence
            then
                Just (chain ++ "/" ++ String.toLower emitter ++ "/" ++ sequence)

            else
                Nothing

        _ ->
            Nothing


decimal : String -> Bool
decimal s =
    not (String.isEmpty s) && String.all Char.isDigit s


{-| A DLN order id: 0x and 32 bytes of hex.
-}
orderId : String -> Maybe String
orderId id =
    if String.startsWith "0x" id && String.length id == 66 then
        dropHexPrefix id
            |> hexOnly
            |> Maybe.map ((++) "0x")

    else
        Nothing


hexOnly : String -> Maybe String
hexOnly s =
    if not (String.isEmpty s) && String.all Char.isHexDigit s then
        Just s

    else
        Nothing


dropHexPrefix : String -> String
dropHexPrefix s =
    if String.startsWith "0x" s || String.startsWith "0X" s then
        String.dropLeft 2 s

    else
        s
