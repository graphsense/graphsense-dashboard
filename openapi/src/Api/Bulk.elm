module Api.Bulk exposing (..)

type alias NeighborEntity =
    { requestEntity : Int
    , requestDirection : Api.Request.Entities.Direction
    , info : String
    , error : String
    , neighbor : Api.Data.NeighborEntity
    }


decodeNeighborEntity : Csv.Decoder NeighborEntity
decodeNeighborEntity =
    Csv.into NeighborEntity
    |> Csv.pipeline (Csv.field "_request_entity" Csv.int)
    |> Csv.pipeline (Csv.field "_request_direction" direction)
    |> Csv.pipeline (Csv.field "_info" Csv.string)
    |> Csv.pipeline (Csv.field "_error" Csv.string)
    |> Csv.pipeline (Csv.field "_error" Csv.string
    -- TODO continue writing decoder


direction : Csv.Decoder Api.Request.Entities.Direction
direction =
    Csv.string
    |> Csv.andThen 
        (\dir ->
            case dir of
                "in" -> Csv.succeed Api.Request.Entities.DirectionIn
                "out" -> Csv.succeed Api.Request.Entities.DirectionOut
                x -> Csv.fail ("unknown direction " ++ x)
        )
