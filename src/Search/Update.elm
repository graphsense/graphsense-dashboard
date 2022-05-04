module Search.Update exposing (update)

import Api.Data
import Bounce
import RemoteData exposing (RemoteData(..))
import Result.Extra as RE
import Search.Effect as Effect exposing (Effect(..), n)
import Search.Model exposing (..)
import Search.Msg exposing (Msg(..))


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        BrowserGotSearchResult result ->
            n
                { model
                    | loading = False
                    , found = Just result
                }

        UserInputsSearch input ->
            ( { model
                | input = input
                , bounce = Bounce.push model.bounce
              }
            , [ BounceEffect 200 RuntimeBounced
              , if model.loading then
                    CancelEffect

                else
                    NoEffect
              ]
                |> BatchEffect
            )

        RuntimeBounced ->
            { model
                | bounce = Bounce.pop model.bounce
            }
                |> n
                |> maybeTriggerSearch


maybeTriggerSearch : ( Model, Effect ) -> ( Model, Effect )
maybeTriggerSearch ( model, cmd ) =
    let
        limit =
            100

        multi =
            String.split "\n" model.input
    in
    if
        Bounce.steady model.bounce
            && (String.length model.input > 3)
            && (List.length multi == 1)
    then
        ( { model
            | loading = True
          }
        , SearchEffect
            { query = model.input
            , currency = Nothing
            , limit = Just limit
            , toMsg = BrowserGotSearchResult
            }
        )

    else
        n model
