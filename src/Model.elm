module Model exposing (..)

import Api.Data
import Browser exposing (UrlRequest)
import Browser.Dom
import Config.UserSettings exposing (UserSettings)
import Config.View
import Dict exposing (Dict)
import Effect.Api
import Effect.Graph
import Effect.Locale
import Effect.Search
import Http
import Json.Encode
import Model.Dialog
import Model.Graph
import Model.Locale
import Model.Search
import Model.Statusbar
import Msg.Graph
import Msg.Locale
import Msg.Search
import Plugin.Model as Plugin
import Plugin.Msg as Plugin
import RemoteData exposing (WebData)
import Time
import Url exposing (Url)


type alias Flags =
    { settings : Json.Encode.Value
    , now : Int
    , width : Int
    , height : Int
    , pluginFlags : Plugin.Flags
    }


type alias Model navigationKey =
    { url : Url
    , key : navigationKey
    , config : Config.View.Config
    , page : Page
    , locale : Model.Locale.Model
    , search : Model.Search.Model
    , graph : Model.Graph.Model
    , user : UserModel
    , stats : WebData Api.Data.Stats
    , width : Int
    , height : Int
    , error : String
    , statusbar : Model.Statusbar.Model
    , dialog : Maybe (Model.Dialog.Model Msg)
    , supportedTokens : Maybe Api.Data.TokenConfigs
    , plugins : Plugin.ModelState --Dict String Json.Encode.Value
    , dirty : Bool
    }


type Page
    = Home
    | Stats
    | Graph
    | Plugin Plugin.PluginType


type Msg
    = NoOp
    | UserRequestsUrl UrlRequest
    | BrowserChangedUrl Url
    | BrowserGotStatistics Api.Data.Stats
    | BrowserGotResponseWithHeaders (Maybe String) (Result ( Http.Error, Effect.Api.Effect Msg ) ( Dict String String, Msg ))
    | UserSwitchesLocale String
    | UserSubmitsApiKeyForm
    | UserInputsApiKeyForm String
    | UserHoversUserIcon String
    | UserLeftUserHovercard
    | UserClickedLayout
    | UserClickedConfirm Msg
    | UserClickedOption Msg
    | UserClickedLogout
    | UserClickedLightmode
    | TimeUpdateReset Time.Posix
    | BrowserGotLoggedOut (Result Http.Error ())
    | BrowserGotElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotContentsElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserChangedWindowSize Int Int
    | BrowserGotEntityTaxonomy (List Api.Data.Concept)
    | BrowserGotAbuseTaxonomy (List Api.Data.Concept)
    | BrowserGotElementForPlugin (Result Browser.Dom.Error Browser.Dom.Element -> Plugin.Msg) (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotSupportedTokens Api.Data.TokenConfigs
    | UserClickedStatusbar
    | UserClosesDialog
    | LocaleMsg Msg.Locale.Msg
    | SearchMsg Msg.Search.Msg
    | GraphMsg Msg.Graph.Msg
    | PluginMsg Plugin.Msg
    | UserClickedExampleSearch String


type RequestLimit
    = Unlimited
    | Limited { remaining : Int, limit : Int, reset : Int }


showResetCounterAtRemaining : Int
showResetCounterAtRemaining =
    20


type alias UserModel =
    { auth : Auth
    , apiKey : String
    , hovercardElement : Maybe Browser.Dom.Element
    }


type Auth
    = Authorized
        { requestLimit : RequestLimit
        , expiration : Maybe Time.Posix
        , loggingOut : Bool
        }
    | Unauthorized Bool (List (Effect.Api.Effect Msg))
    | Unknown


type Effect
    = NavLoadEffect String
    | NavPushUrlEffect String
    | GetElementEffect { id : String, msg : Result Browser.Dom.Error Browser.Dom.Element -> Msg }
    | GetContentsElementEffect
    | LocaleEffect Effect.Locale.Effect
    | SearchEffect Effect.Search.Effect
    | GraphEffect Effect.Graph.Effect
    | ApiEffect (Effect.Api.Effect Msg)
    | PluginEffect (Cmd Plugin.Msg)
    | PortsConsoleEffect String
    | CmdEffect (Cmd Msg)
    | LogoutEffect
    | SetDirtyEffect
    | SetCleanEffect
    | SaveUserSettingsEffect UserSettings


type Thing
    = Address Api.Data.Address
    | Entity Api.Data.Entity


userSettingsFromMainModel : Model key -> UserSettings
userSettingsFromMainModel model =
    { selectedLanguage = model.locale.locale
    , lightMode = Just model.config.lightmode
    , valueDetail = Just model.locale.valueDetail
    , valueDenomination = Just model.locale.currency
    , addressLabel = Just model.graph.config.addressLabelType
    , edgeLabel = Just model.graph.config.txLabelType
    , showAddressShadowLinks = Just model.graph.config.showAddressShadowLinks
    , showClusterShadowLinks = Just model.graph.config.showEntityShadowLinks
    , showDatesInUserLocale = Just model.graph.config.showDatesInUserLocale
    }


getLatestBlocks : WebData Api.Data.Stats -> List ( String, Int )
getLatestBlocks =
    RemoteData.map .currencies
        >> RemoteData.withDefault []
        >> List.map (\{ name, noBlocks } -> ( name, noBlocks - 1 ))
