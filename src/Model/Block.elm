module Model.Block exposing (Block, decoder, encoder)

import Api.Data
import Json.Decode
import Json.Encode


type alias Block =
    { currency : String
    , block : Int
    }


decoder : Json.Decode.Decoder Block
decoder =
    Json.Decode.map2 Block
        (Json.Decode.field "currency" Json.Decode.string)
        (Json.Decode.field "block" Json.Decode.int)


encoder : Api.Data.Block -> Json.Encode.Value
encoder block =
    Json.Encode.object
        [ ( "currency", Json.Encode.string block.currency )
        , ( "block", Json.Encode.int block.height )
        ]
