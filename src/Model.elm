module Model exposing (..)

import Api.Data
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Http
import Locale.Effect
import Locale.Model
import Locale.Msg
import RemoteData exposing (WebData)
import Search.Effect
import Search.Model
import Search.Msg
import Theme.Theme exposing (Theme)
import Time
import Url exposing (Url)


type alias Flags =
    { locale : String
    }


type alias Config =
    { theme : Theme
    }


type alias Model navigationKey =
    { url : Url
    , key : navigationKey
    , locale : Locale.Model.Model
    , search : Search.Model.Model
    , user : UserModel
    , stats : WebData Api.Data.Stats
    }


type Msg
    = UserRequestsUrl UrlRequest
    | BrowserChangedUrl Url
    | BrowserGotStatistics (Result Http.Error Api.Data.Stats)
    | BrowserGotResponseWithHeaders (Result ( Http.Error, Effect ) ( Dict String String, Msg ))
    | LocaleMsg Locale.Msg.Msg
    | SearchMsg Search.Msg.Msg
    | UserSwitchesLocale String
    | UserSubmitsApiKeyForm
    | UserInputsApiKeyForm String
    | UserHoversUserIcon String
    | UserLeftUserHovercard
    | BrowserGotElement (Result Dom.Error Dom.Element)


type RequestLimit
    = Unlimited
    | Limited { remaining : Int, limit : Int, reset : Int }


type alias UserModel =
    { auth : Auth
    , apiKey : String
    , hovercardElement : Maybe Dom.Element
    }


type Auth
    = Authorized
        { requestLimit : RequestLimit
        , expiration : Maybe Time.Posix
        }
    | Unauthorized (List Effect)
    | Loading
    | Unknown


type Effect
    = NoEffect
    | NavLoadEffect String
    | NavPushUrlEffect String
    | GetStatisticsEffect
    | GetElementEffect { id : String, msg : Result Dom.Error Dom.Element -> Msg }
    | BatchedEffects (List Effect)
    | LocaleEffect Locale.Effect.Effect
    | SearchEffect Search.Effect.Effect


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


batch : List Effect -> Effect
batch effs =
    BatchedEffects effs
