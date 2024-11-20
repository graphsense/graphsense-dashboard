module Update.Graph.Search exposing (Config, selectCategory, selectCriterion, selectDirection, submit)

import Api.Data
import Api.Request.Entities
import Effect.Api
import Effect.Graph exposing (Effect(..))
import Init.Graph.Search exposing (initCriterion)
import Model.Graph.Id as Id
import Model.Graph.Search exposing (..)
import Msg.Graph exposing (Msg(..))
import Util exposing (n)


type alias Config =
    { categories : List Api.Data.Concept
    }


selectCriterion : Config -> String -> Model -> ( Model, List Effect )
selectCriterion config criterion model =
    { model
        | criterion =
            initCriterion config.categories
    }
        |> n


selectCategory : String -> Model -> ( Model, List Effect )
selectCategory category model =
    case model.criterion of
        Category categories _ ->
            { model
                | criterion = Category categories category
            }
                |> n


selectDirection : String -> Model -> ( Model, List Effect )
selectDirection direction model =
    { model
        | direction =
            case direction of
                "incoming" ->
                    Incoming

                "outgoing" ->
                    Outgoing

                "both" ->
                    Both

                _ ->
                    Outgoing
    }
        |> n


submit : { depth : Int, breadth : Int, maxAddresses : Int } -> Model -> ( Model, List Effect )
submit { depth, breadth, maxAddresses } model =
    let
        ( key, value ) =
            case model.criterion of
                Category _ active ->
                    ( Api.Request.Entities.KeyCategory
                    , [ active ]
                    )

        makeEffect isOutgoing =
            BrowserGotEntitySearchResult model.id isOutgoing
                |> Effect.Api.SearchEntityNeighborsEffect
                    { currency = Id.currency model.id
                    , entity = Id.entityId model.id
                    , isOutgoing = isOutgoing
                    , key = key
                    , value = value
                    , depth = depth
                    , breadth = breadth
                    , maxAddresses = maxAddresses
                    }
                |> ApiEffect
    in
    ( model
    , case model.direction of
        Outgoing ->
            [ makeEffect True ]

        Incoming ->
            [ makeEffect False ]

        Both ->
            [ makeEffect True
            , makeEffect False
            ]
    )
