module CIString exposing (CIString, equalsString, fromString, toString)


type CIString
    = CIString String


fromString : String -> CIString
fromString =
    CIString


toString : CIString -> String
toString (CIString s) =
    s


equalsString : String -> CIString -> Bool
equalsString s (CIString s_) =
    String.toLower s == String.toLower s_
