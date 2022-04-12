module View.Env exposing (Env, defaultEnv)


type alias Env =
    { getString : String -> String
    , scaled : Float -> Float
    }


defaultEnv : Env
defaultEnv =
    { getString = identity
    , scaled = (*) 10
    }
