module Update.Pathfinder.WorkflowNextUtxoTx exposing (update)

import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..), WorkflowNextTxContext, WorkflowNextUtxoTxMsg(..))


update : WorkflowNextTxContext -> WorkflowNextUtxoTxMsg -> Model -> ( Model, List Effect )
update context msg model =
    case msg of
        BrowserGotReferencedTxs refs ->
            ( model
            , refs
                |> List.map
                    (\ref ->
                        BrowserGotTxForAddress context.addressId context.direction
                            |> Api.GetTxEffect
                                { currency = Id.network context.addressId
                                , txHash = ref.txHash
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                    )
            )
