module Update.Pathfinder.SearchBox exposing (Context, close, open, refreshMatches, update)

import Api.Data
import Dict exposing (Dict)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.SearchBox exposing (Model, empty)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Msg.Pathfinder.SearchBox exposing (Msg(..))
import RemoteData
import Set
import Util.Annotations as Annotations


type alias Context =
    { network : Network
    , annotations : Annotations.AnnotationModel
    , tagSummaries : Dict Id HavingTags
    }


update : Msg -> Context -> Model -> Model
update msg ctx model =
    case msg of
        UserChangedQuery q ->
            { model | query = q }
                |> recompute ctx

        UserClickedNext ->
            advance 1 model

        UserClickedPrev ->
            advance -1 model

        UserPressedEnterInBox ->
            advance 1 model

        UserClickedClose ->
            close


open : Context -> Model -> Model
open ctx model =
    { model | visible = True }
        |> recompute ctx


close : Model
close =
    empty


refreshMatches : Context -> Model -> Model
refreshMatches ctx model =
    if model.visible then
        recompute ctx model

    else
        model


recompute : Context -> Model -> Model
recompute ctx model =
    let
        q =
            String.toLower (String.trim model.query)

        addressMatches =
            if String.isEmpty q then
                []

            else
                ctx.network.addresses
                    |> Dict.values
                    |> List.filter (addressMatchesQuery q ctx)
                    |> List.map .id

        txMatches =
            if String.isEmpty q then
                []

            else
                ctx.network.txs
                    |> Dict.values
                    |> List.filter (txMatchesQuery q ctx)
                    |> List.map .id

        matches =
            addressMatches ++ txMatches

        currentIdx =
            if List.isEmpty matches then
                Nothing

            else
                Just 0
    in
    { model
        | matches = matches
        , matchSet = Set.fromList matches
        , currentMatchIndex = currentIdx
    }


addressMatchesQuery : String -> Context -> Address -> Bool
addressMatchesQuery q ctx address =
    let
        addrStr =
            address.data
                |> RemoteData.toMaybe
                |> Maybe.map (.address >> String.toLower)
                |> Maybe.withDefault (String.toLower (Id.id address.id))

        idMatch =
            String.contains q addrStr

        exchangeMatch =
            address.exchange
                |> Maybe.map (String.toLower >> String.contains q)
                |> Maybe.withDefault False

        actorMatch =
            address.actor
                |> Maybe.map (String.toLower >> String.contains q)
                |> Maybe.withDefault False
    in
    idMatch
        || exchangeMatch
        || actorMatch
        || annotationMatches q ctx.annotations address.id
        || tagSummaryMatches q ctx.tagSummaries address.id


txMatchesQuery : String -> Context -> Tx -> Bool
txMatchesQuery q ctx tx =
    let
        hashMatch =
            Tx.getRawBaseTxHashForTx tx
                |> String.toLower
                |> String.contains q
    in
    hashMatch || annotationMatches q ctx.annotations tx.id


annotationMatches : String -> Annotations.AnnotationModel -> Id -> Bool
annotationMatches q annotations id =
    Annotations.getAnnotation id annotations
        |> Maybe.map (.label >> String.toLower >> String.contains q)
        |> Maybe.withDefault False


tagSummaryMatches : String -> Dict Id HavingTags -> Id -> Bool
tagSummaryMatches q tagSummaries id =
    case Dict.get id tagSummaries of
        Just (HasTagSummaryWithCluster ts) ->
            tagSummaryStringMatches q ts

        Just (HasTagSummaryWithoutCluster ts) ->
            tagSummaryStringMatches q ts

        Just (HasTagSummaryOnlyWithCluster ts) ->
            tagSummaryStringMatches q ts

        Just (HasTagSummaries { withCluster, withoutCluster }) ->
            tagSummaryStringMatches q withCluster || tagSummaryStringMatches q withoutCluster

        _ ->
            False


tagSummaryStringMatches : String -> Api.Data.TagSummary -> Bool
tagSummaryStringMatches q ts =
    let
        contains s =
            String.contains q (String.toLower s)

        bestLabelMatch =
            ts.bestLabel |> Maybe.map contains |> Maybe.withDefault False

        broadCategoryMatch =
            contains ts.broadCategory

        conceptKeyMatch =
            ts.conceptTagCloud
                |> Dict.keys
                |> List.any contains

        labelSummaryMatch =
            ts.labelSummary
                |> Dict.toList
                |> List.any
                    (\( labelKey, ls ) ->
                        contains labelKey
                            || contains ls.label
                            || List.any contains ls.concepts
                    )
    in
    bestLabelMatch || broadCategoryMatch || conceptKeyMatch || labelSummaryMatch


advance : Int -> Model -> Model
advance step model =
    if List.isEmpty model.matches then
        model

    else
        let
            total =
                List.length model.matches

            current =
                Maybe.withDefault 0 model.currentMatchIndex

            next =
                modBy total (current + step)
        in
        { model | currentMatchIndex = Just next }
