module Update.Graph.Tag exposing (..)

import Effect.Graph as Graph exposing (Effect)
import Model.Graph.Tag exposing (..)
import Model.Search as Search
import Msg.Search as Search
import RecordSetter exposing (..)
import Tuple exposing (..)
import Update.Search as Search


searchMsg : Search.Msg -> Model -> ( Model, List Effect )
searchMsg msg model =
    let
        ( search, eff ) =
            case msg of
                Search.UserClicksResult ->
                    ( model.input.label, [] )

                Search.UserClicksResultLine (Search.Label lb) ->
                    Search.update msg model.input.label
                        -- add back the input string
                        |> mapFirst (s_input lb)

                _ ->
                    Search.update msg model.input.label
    in
    ( { model
        | input =
            model.input
                |> s_label search
      }
    , List.map Graph.TagSearchEffect eff
    )


inputSource : String -> Model -> Model
inputSource input model =
    { model
        | input =
            model.input
                |> s_source input
    }


inputCategory : String -> Model -> Model
inputCategory input model =
    { model
        | input =
            model.input
                |> s_category input
    }


inputAbuse : String -> Model -> Model
inputAbuse input model =
    { model
        | input =
            model.input
                |> s_abuse input
    }
