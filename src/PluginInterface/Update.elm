module PluginInterface.Update exposing (Return, Update, UserInfo, andThen, init)

import Config.Update as Update
import PluginInterface.Msg exposing (InMsg, OutMsg)
import Set exposing (Set)
import Time


type alias Return modelState msg addressMsg entityMsg =
    ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg )


{-| What the user endpoint (`VITE_GS_USER_ENDPOINT_URL`, the portal's
`/user`) reported about the logged-in user. Each field is Nothing when the
endpoint does not say.
-}
type alias UserInfo =
    { username : Maybe String
    , expiration : Maybe Time.Posix
    , hasEmail : Maybe Bool
    }


type alias Update flags modelState addressState msg addressMsg entityMsg =
    { -- update plugin's state
      update : Maybe (Update.Config -> msg -> modelState -> Return modelState msg addressMsg entityMsg)

    -- update by core msg
    , updateByCoreMsg : Maybe (Update.Config -> InMsg -> modelState -> Return modelState msg addressMsg entityMsg)

    -- update an address's plugin state
    , updateAddress : Maybe (addressMsg -> addressState -> addressState)

    -- update by change of URL
    , updateByUrl : Maybe (Update.Config -> String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- when user inputs an API key, process the sha256 hash of the key
    , updateApiKeyHash : Maybe (String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- when user inputs an API key
    , updateApiKey : Maybe (String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- initialize plugin's state
    , init : Maybe (flags -> Return modelState msg addressMsg entityMsg)

    -- initialize plugin state on init of address
    , initAddress : Maybe addressState

    -- when the search results of the search bar need to be cleared
    , clearSearch : Maybe (modelState -> modelState)

    -- when the user logs out
    , logout : Maybe (modelState -> Return modelState msg addressMsg entityMsg)

    -- when the user info arrived from the user endpoint (once per session,
    -- after the first successful API response)
    , updateByUserInfo : Maybe (UserInfo -> modelState -> Return modelState msg addressMsg entityMsg)

    -- plugin can decide when core should store a history entry, before a plugin action.
    , shallPushHistory : Maybe (msg -> Bool)
    }


init : Update flags modelState addressState msg addressMsg entityMsg
init =
    { update = Nothing
    , updateByCoreMsg = Nothing
    , updateAddress = Nothing
    , updateByUrl = Nothing
    , updateApiKeyHash = Nothing
    , updateApiKey = Nothing
    , init = Nothing
    , initAddress = Nothing
    , clearSearch = Nothing
    , logout = Nothing
    , updateByUserInfo = Nothing
    , shallPushHistory = Nothing
    }


andThen : (modelState -> Return modelState msg addressMsg entityMsg) -> Return modelState msg addressMsg entityMsg -> Return modelState msg addressMsg entityMsg
andThen fun ( modelA, outMsgA, cmdA ) =
    let
        ( modelB, outMsgB, cmdB ) =
            fun modelA
    in
    ( modelB, outMsgA ++ outMsgB, Cmd.batch [ cmdA, cmdB ] )
