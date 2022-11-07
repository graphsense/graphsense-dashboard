module Util.Csv exposing (..)

import Api.Data
import Json.Encode


int : Int -> String
int =
    String.fromInt


bool : Bool -> String
bool b =
    if b then
        "true"

    else
        "false"


string : String -> String
string =
    identity


float : Float -> String
float =
    String.fromFloat


prefix : String -> String -> ( String, List String )
prefix key key2 =
    ( key ++ "_" ++ key2, [] )


values : String -> Api.Data.Values -> List ( ( String, List String ), String )
values key v =
    ( ( key, [] ), int v.value )
        :: List.map (\f -> ( prefix key f.code, float f.value )) v.fiatValues


a0 : String -> String
a0 s =
    s ++ " {0}"
