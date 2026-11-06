module Scenario.ReportTagDialogTest exposing (suite)

{-| The report tag dialog sends an optional wallet type as a concept. The field
has to stay out of the request when nothing is picked or the pick is "Other",
which has no concept, and a second click takes a pick back.
-}

import Api.Data
import Effect.Api as Api
import Expect
import Init.Pathfinder.Id as Id
import Init.Search as Search
import Json.Encode
import Model exposing (AddTagDialogMsgs(..), Effect(..), Msg(..))
import Model.Dialog as Dialog
import Model.Pathfinder.Id exposing (Id)
import Model.Search
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


reportedConcepts : App -> List (Maybe (List String))
reportedConcepts =
    App.effects
        >> List.filterMap
            (\eff ->
                case eff of
                    ApiEffect (Api.AddUserReportedTag tag _) ->
                        Just tag.concepts

                    _ ->
                        Nothing
            )


id : Id
id =
    Id.init "btc" "1Archive1n2C579dMsAu3iC6tWzuQJz8dN"


dialog : App
dialog =
    App.initAt "/pathfinder"
        |> App.mapModel
            (\m ->
                { m
                    | dialog =
                        Dialog.AddTag
                            { id = id
                            , closeMsg = UserClosesDialog
                            , addTagMsg = AddTagDialog (UserClickedAddTag id)
                            , search = Search.init Model.Search.SearchActorsOnly
                            , selectedActor = Just ( "binance", "Binance" )
                            , walletType = Nothing
                            , description = ""
                            }
                            |> Just
                }
            )


pick : String -> Msg
pick =
    Dialog.WalletConcept >> pickType


pickType : Dialog.WalletType -> Msg
pickType =
    UserClickedWalletType >> AddTagDialog


send : Msg
send =
    AddTagDialog (UserClickedAddTag id)


encoded : Maybe (List String) -> String
encoded concepts =
    { actor = "binance", address = "a", concepts = concepts, description = "", label = "Binance", network = "btc" }
        |> Api.Data.encodeUserReportedTag
        |> Json.Encode.encode 0


suite : Test
suite =
    describe "report tag dialog"
        [ test "shows the wallet types, by id until the taxonomy has loaded" <|
            \_ ->
                dialog
                    |> App.html
                    |> Expect.all
                        ([ "hot_wallet", "cold_wallet", "deposit_wallet", "Other" ]
                            |> List.map (\label -> Query.has [ Selector.text label ])
                        )
        , test "sends the picked wallet type as a concept" <|
            \_ ->
                dialog
                    |> App.steps [ pick "hot_wallet", send ]
                    |> reportedConcepts
                    |> Expect.equal [ Just [ "hot_wallet" ] ]
        , test "the last pick wins" <|
            \_ ->
                dialog
                    |> App.steps [ pick "hot_wallet", pick "deposit_wallet", send ]
                    |> reportedConcepts
                    |> Expect.equal [ Just [ "deposit_wallet" ] ]
        , test "a second click takes the pick back" <|
            \_ ->
                dialog
                    |> App.steps [ pick "cold_wallet", pick "cold_wallet", send ]
                    |> reportedConcepts
                    |> Expect.equal [ Nothing ]
        , test "other has no concept to send" <|
            \_ ->
                dialog
                    |> App.steps [ pickType Dialog.OtherWallet, send ]
                    |> reportedConcepts
                    |> Expect.equal [ Nothing ]
        , test "other replaces a wallet type picked before" <|
            \_ ->
                dialog
                    |> App.steps [ pick "hot_wallet", pickType Dialog.OtherWallet, send ]
                    |> reportedConcepts
                    |> Expect.equal [ Nothing ]
        , test "without a pick the request has no concepts" <|
            \_ ->
                dialog
                    |> App.step send
                    |> reportedConcepts
                    |> Expect.equal [ Nothing ]
        , test "the request names the field as the API does" <|
            \_ ->
                [ encoded Nothing |> String.contains "concepts"
                , encoded (Just [ "hot_wallet" ]) |> String.contains "\"concepts\":[\"hot_wallet\"]"
                ]
                    |> Expect.equal [ False, True ]
        ]
