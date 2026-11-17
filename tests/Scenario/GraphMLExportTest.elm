module Scenario.GraphMLExportTest exposing (suite)

{-| GraphML is the export dialog's one format the browser cannot write: the
graph goes to the backend's converter (POST /graph/convert), and the reply is
the file. Pinned here: what the request carries (the whole graph or only the
selection, and the two GraphML options), that a reply downloads and closes the
dialog, that a failure leaves the dialog usable, and the backend's node cap.
-}

import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Network
import Dict
import Effect.Api as Api
import Effect.Pathfinder
import Expect
import Html.Attributes
import Http
import Json.Encode
import Model exposing (Effect(..), Msg(..))
import Model.Dialog as Dialog
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Msg.ExportDialog as ExportDialog
import Msg.Pathfinder as Pathfinder
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


type alias Request =
    { graph : String
    , aggregate : Bool
    , includeTags : Bool
    , reply : String -> Pathfinder.Msg
    }


{-| The two connected addresses and their tx on the graph, the export dialog
open.
-}
dialogWith : Selection -> App
dialogWith selection =
    App.initAt "/pathfinder"
        |> App.mapModel
            (\m ->
                let
                    pf =
                        m.pathfinder
                in
                { m | pathfinder = { pf | network = Network.twoConnectedAddresses, selection = selection } }
            )
        |> App.step (PathfinderMsg (Pathfinder.UserClickedExportGraph (Just (Time.millisToPosix 0))))


dialog : App
dialog =
    dialogWith NoSelection


exportMsg : ExportDialog.Msg -> Msg
exportMsg =
    ExportDialogMsg


pickGraphML : Msg
pickGraphML =
    exportMsg (ExportDialog.UserClickedFormatOption Dialog.ExportFormatGraphML)


export : Msg
export =
    exportMsg ExportDialog.UserClickedExport


requests : App -> List Request
requests =
    App.effects
        >> List.filterMap
            (\eff ->
                case eff of
                    PathfinderEffect (Effect.Pathfinder.ApiEffect (Api.ConvertGraphToGraphMLEffect r reply)) ->
                        Just
                            { graph = Json.Encode.encode 0 r.graph
                            , aggregate = r.aggregate
                            , includeTags = r.includeTags
                            , reply = reply
                            }

                    _ ->
                        Nothing
            )


exportConfig : App -> Maybe (Dialog.ExportConfig Msg)
exportConfig app =
    case (App.model app).dialog of
        Just (Dialog.Export conf) ->
            Just conf

        _ ->
            Nothing


{-| The scenario runs without translations, so the view shows the key.
-}
tooManyNodes : String
tooManyNodes =
    "Export-dialog-graphml-too-many-nodes"


clickExportButton : App -> Result String Msg
clickExportButton =
    App.html
        >> Query.find [ Selector.attribute (Html.Attributes.attribute "role" "button"), Selector.containing [ Selector.text "Export-dialog-button" ] ]
        >> Event.simulate Event.click
        >> Event.toResult


suite : Test
suite =
    describe "GraphML export"
        [ test "the dialog offers GraphML" <|
            \_ ->
                dialog
                    |> App.html
                    |> Query.has [ Selector.text "GraphML" ]
        , test "exporting sends the whole graph to the converter, with tags, one node per tx" <|
            \_ ->
                dialog
                    |> App.steps [ pickGraphML, export ]
                    |> requests
                    |> List.map (\r -> ( r.aggregate, r.includeTags, List.map (\( _, a ) -> String.contains a r.graph) [ Id.address1, Id.address3, Id.tx1 ] ))
                    |> Expect.equal [ ( False, True, [ True, True, True ] ) ]
        , test "the two options reach the request" <|
            \_ ->
                dialog
                    |> App.steps
                        [ pickGraphML
                        , exportMsg ExportDialog.UserClickedGraphMLAggregate
                        , exportMsg ExportDialog.UserClickedGraphMLIncludeTags
                        , export
                        ]
                    |> requests
                    |> List.map (\r -> ( r.aggregate, r.includeTags ))
                    |> Expect.equal [ ( True, False ) ]
        , test "with a selection, only the selected nodes are sent" <|
            \_ ->
                dialogWith (MultiSelect [ MSelectedAddress Id.address1, MSelectedTx Id.tx1 ])
                    |> App.steps [ pickGraphML, export ]
                    |> requests
                    |> List.map (\r -> List.map (\( _, a ) -> String.contains a r.graph) [ Id.address1, Id.address3, Id.tx1 ])
                    |> Expect.equal [ [ True, False, True ] ]
        , test "the visible area, which a graph format does not have, becomes the whole graph" <|
            \_ ->
                dialog
                    |> App.steps [ exportMsg (ExportDialog.UserClickedAreaOption Dialog.ExportAreaVisible), pickGraphML ]
                    |> exportConfig
                    |> Maybe.map .area
                    |> Expect.equal (Just Dialog.ExportAreaWhole)
        , test "the file name takes the GraphML extension, and gives it back" <|
            \_ ->
                dialog
                    |> App.steps [ exportMsg (ExportDialog.UserInputsFilename "case.pdf"), pickGraphML ]
                    |> (\app ->
                            [ exportConfig app
                            , App.step (exportMsg (ExportDialog.UserClickedFormatOption Dialog.ExportFormatPNG)) app
                                |> exportConfig
                            ]
                       )
                    |> List.map (Maybe.map .filename)
                    |> Expect.equal [ Just "case.graphml", Just "case.png" ]
        , test "the reply downloads the file and closes the dialog" <|
            \_ ->
                let
                    exported =
                        dialog |> App.steps [ exportMsg (ExportDialog.UserInputsFilename "case.graphml"), pickGraphML, export ]
                in
                case requests exported of
                    [ r ] ->
                        exported
                            |> App.step (PathfinderMsg (r.reply "<graphml/>"))
                            |> Expect.all
                                [ exportConfig >> Expect.equal Nothing
                                , App.expectEffect "the download"
                                    (\eff ->
                                        case eff of
                                            PathfinderEffect (Effect.Pathfinder.CmdEffect _) ->
                                                True

                                            _ ->
                                                False
                                    )
                                ]

                    rs ->
                        Expect.fail ("expected one request, got " ++ String.fromInt (List.length rs))
        , test "a failed conversion leaves the dialog open and usable" <|
            \_ ->
                let
                    failed =
                        Api.ConvertGraphToGraphMLEffect { graph = Json.Encode.null, aggregate = False, includeTags = True } (always NoOp)
                in
                dialog
                    |> App.steps
                        [ pickGraphML
                        , export
                        , BrowserGotResponseWithHeaders Nothing (Err ( Http.BadStatus 500, Dict.empty, failed ))
                        ]
                    |> exportConfig
                    |> Maybe.map .exporting
                    |> Expect.equal (Just False)
        , describe "above the backend's 500 nodes"
            (let
                tooBig =
                    dialog
                        |> App.step pickGraphML
                        |> App.mapModel
                            (\m ->
                                case m.dialog of
                                    Just (Dialog.Export conf) ->
                                        { m | dialog = Just (Dialog.Export { conf | nodeCounts = { whole = 501, selected = 0 } }) }

                                    _ ->
                                        m
                            )
             in
             [ test "says so" <|
                \_ ->
                    tooBig
                        |> App.html
                        |> Query.has [ Selector.text tooManyNodes ]
             , test "the export button does not react" <|
                \_ ->
                    tooBig
                        |> clickExportButton
                        |> Expect.err
             , test "PDF is not capped" <|
                \_ ->
                    tooBig
                        |> App.step (exportMsg (ExportDialog.UserClickedFormatOption Dialog.ExportFormatPDF))
                        |> Expect.all
                            [ App.html >> Query.hasNot [ Selector.text tooManyNodes ]
                            , clickExportButton >> Expect.equal (Ok export)
                            ]
             ]
            )
        ]
