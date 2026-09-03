module Scenario.OpenSelectionInNewTabTest exposing (suite)

{-| The address context menu's "Open in new tab" entry is enabled on a
multi-selection and then hands the whole selection to the browser. The
hand-over itself is a port, so the browser layer is where the new tab is
checked; here we pin that the entry is offered and that the command is issued.
-}

import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Network
import Effect.Pathfinder exposing (Effect(..))
import Expect
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Msg.Pathfinder exposing (Msg(..))
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


withMenu : Selection -> App
withMenu selection =
    App.init
        |> App.mapModel
            (\m ->
                { m
                    | network = Network.twoConnectedAddresses
                    , selection = selection
                    , contextMenu = Just ( { x = 10, y = 10 }, ContextMenu.AddressContextMenu Id.address1 )
                }
            )


multi : Selection
multi =
    MultiSelect [ MSelectedAddress Id.address1, MSelectedAddress Id.address3 ]


isCmd : Effect -> Bool
isCmd eff =
    case eff of
        CmdEffect _ ->
            True

        _ ->
            False


suite : Test
suite =
    describe "open selection in new tab"
        [ test "the entry is on the menu of a multi-selection" <|
            \_ ->
                withMenu multi
                    |> App.html
                    |> Query.has [ Selector.text "Open in new tab" ]
        , test "clicking it hands the selection to the browser" <|
            \_ ->
                withMenu multi
                    |> App.step (UserClickedContextMenuOpenInNewTab (ContextMenu.AddressContextMenu Id.address1))
                    |> App.expectEffect "the openGraphInNewTab port" isCmd
        , test "and closes the menu" <|
            \_ ->
                withMenu multi
                    |> App.step (UserClickedContextMenuOpenInNewTab (ContextMenu.AddressContextMenu Id.address1))
                    |> App.model
                    |> .contextMenu
                    |> Expect.equal Nothing
        ]
