module Plugin exposing (..)

import Json.Encode exposing (Value)
import Model.Graph.Address as Address
import Svg exposing (..)


type Place
    = Model
    | Address


type alias Plugin =
    { view :
        { graph :
            { address :
                { flags : Value -> Address.Address -> Svg Value
                }
            }
        }
    , update :
        { model : Value -> Value -> ( Value, Cmd Value )
        , graph :
            { address : Value -> Value -> ( Value, Cmd Value )
            }
        }
    }
