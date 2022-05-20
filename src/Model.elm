module Model exposing (..)

import Api.Data
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Config.View
import Dict exposing (Dict)
import Effect.Graph
import Effect.Locale
import Effect.Search
import Html exposing (Attribute, Html)
import Http
import Json.Encode
import Model.Graph
import Model.Locale
import Model.Search
import Msg.Graph
import Msg.Locale
import Msg.Search
import Page
import Plugin.Model as Plugin
import RemoteData exposing (WebData)
import Theme.Theme exposing (Theme)
import Time
import Url exposing (Url)


type alias Flags =
    { locale : String
    , now : Int
    , width : Int
    , height : Int
    }


type alias Model navigationKey =
    { url : Url
    , key : navigationKey
    , config : Config.View.Config
    , page : Page.Page
    , locale : Model.Locale.Model
    , search : Model.Search.Model
    , graph : Model.Graph.Model
    , user : UserModel
    , stats : WebData Api.Data.Stats
    , width : Int
    , height : Int
    , error : String
    , plugins : Dict String Json.Encode.Value
    }


type Msg
    = UserRequestsUrl UrlRequest
    | BrowserChangedUrl Url
    | BrowserGotStatistics (Result Http.Error Api.Data.Stats)
    | BrowserGotResponseWithHeaders (Result ( Http.Error, Effect ) ( Dict String String, Msg ))
    | UserSwitchesLocale String
    | UserSubmitsApiKeyForm
    | UserInputsApiKeyForm String
    | UserHoversUserIcon String
    | UserLeftUserHovercard
    | BrowserGotElement (Result Dom.Error Dom.Element)
    | BrowserChangedWindowSize Int Int
    | LocaleMsg Msg.Locale.Msg
    | SearchMsg Msg.Search.Msg
    | GraphMsg Msg.Graph.Msg


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
    = NavLoadEffect String
    | NavPushUrlEffect String
    | GetStatisticsEffect
    | GetElementEffect { id : String, msg : Result Dom.Error Dom.Element -> Msg }
    | LocaleEffect Effect.Locale.Effect
    | SearchEffect Effect.Search.Effect
    | GraphEffect Effect.Graph.Effect
    | PortsConsoleEffect String


type Thing
    = Address Api.Data.Address
    | Entity Api.Data.Entity
