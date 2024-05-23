module Util exposing (ensureAndSimulateHttp)

import Json.Encode
import ProgramTest exposing (ProgramTest)


ensureAndSimulateHttp : String -> String -> a -> (a -> Json.Encode.Value) -> ProgramTest model msg effect -> ProgramTest model msg effect
ensureAndSimulateHttp method url a encode =
    ProgramTest.ensureHttpRequestWasMade method url
        >> ProgramTest.simulateHttpOk method
            url
            (a |> encode |> Json.Encode.encode 0)
