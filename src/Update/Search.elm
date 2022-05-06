module Update.Search exposing (update)

import Api.Data
import Bounce
import Effect exposing (n)
import Effect.Search as Effect exposing (Effect(..))
import Model.Search exposing (..)
import Msg.Search exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Result.Extra as RE


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        BrowserGotSearchResult result ->
            n
                { model
                    | loading = False
                    , found = Just result
                }

        UserClicksResultLine ->
            n
                { model
                    | found = Nothing
                }

        UserInputsSearch input ->
            ( { model
                | input = input
                , bounce = Bounce.push model.bounce
              }
            , BounceEffect 200 RuntimeBounced
                :: (if model.loading then
                        [ CancelEffect ]

                    else
                        []
                   )
            )

        RuntimeBounced ->
            { model
                | bounce = Bounce.pop model.bounce
            }
                |> n
                |> maybeTriggerSearch


maybeTriggerSearch : ( Model, List Effect ) -> ( Model, List Effect )
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
            :: cmd
        )

    else
        n model
