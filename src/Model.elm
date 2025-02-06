module Model exposing (Auth(..), Effect(..), Flags, Model, Msg(..), Page(..), RequestLimit(..), SettingsMsg(..), Thing(..), UserModel, showResetCounterAtRemaining, userSettingsFromMainModel)

import Api.Data
import Browser exposing (UrlRequest)
import Browser.Dom
import Config.UserSettings exposing (UserSettings)
import Config.View
import Dict exposing (Dict)
import Effect.Api
import Effect.Graph
import Effect.Locale
import Effect.Pathfinder
import Effect.Search
import Hovercard
import Http
import Json.Encode
import Model.Dialog
import Model.Graph
import Model.Notification
import Model.Pathfinder
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType)
import Model.Search
import Model.Statusbar
import Msg.Graph
import Msg.Locale
import Msg.Pathfinder
import Msg.Search
import Plugin.Model as Plugin
import Plugin.Msg as Plugin
import RemoteData exposing (WebData)
import Table
import Time
import Url exposing (Url)
import Util.ThemedSelectBox as SelectBox
import Util.ThemedSelectBoxes as SelectBoxes


type alias Flags =
    { localStorage : Json.Encode.Value
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
    , search : Model.Search.Model
    , graph : Model.Graph.Model
    , pathfinder : Model.Pathfinder.Model
    , user : UserModel
    , stats : WebData Api.Data.Stats
    , width : Int
    , height : Int
    , error : String
    , statusbar : Model.Statusbar.Model
    , dialog : Maybe (Model.Dialog.Model Msg)
    , supportedTokens : Dict String Api.Data.TokenConfigs
    , plugins : Plugin.ModelState --Dict String Json.Encode.Value
    , notifications : Model.Notification.Model
    , selectBoxes : SelectBoxes.Model
    , dirty : Bool
    , tooltip : Maybe (Tooltip Msg)
    }


type Page
    = Home
    | Stats
    | Settings
    | Graph
    | Pathfinder
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
    | UserClickedUserIcon String
    | UserLeftUserHovercard
    | UserClickedLayout
    | UserClickedConfirm Msg
    | UserClickedOption Msg
    | UserClickedOutsideDialog Msg
    | UserClickedLogout
    | UserClickedLightmode
    | TimeUpdateReset Time.Posix
    | BrowserGotLoggedOut (Result Http.Error ())
    | BrowserGotContentsElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserChangedWindowSize Int Int
    | BrowserGotEntityTaxonomy (List Api.Data.Concept)
    | BrowserGotAbuseTaxonomy (List Api.Data.Concept)
    | BrowserGotElementForPlugin (Result Browser.Dom.Error Browser.Dom.Element -> Plugin.Msg) (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotSupportedTokens String Api.Data.TokenConfigs
    | UserClickedStatusbar
    | UserClosesDialog
    | TagsListDialogTableUpdateMsg Table.State
    | LocaleMsg Msg.Locale.Msg
    | SearchMsg Msg.Search.Msg
    | GraphMsg Msg.Graph.Msg
    | PathfinderMsg Msg.Pathfinder.Msg
    | PluginMsg Plugin.Msg
    | UserClickedExampleSearch String
    | UserHovercardMsg Hovercard.Msg
    | UserClosesNotification
    | SettingsMsg SettingsMsg
    | SelectBoxMsg SelectBoxes.SelectBoxesAvailable SelectBox.Msg
    | UserClickedNavBack
    | UserClickedNavHome
    | NotificationMsg Model.Notification.Msg
    | RuntimePostponedUpdateByUrl Url
    | OpenTooltip String (TooltipType Msg)
    | ClosingTooltip (Maybe String) Bool
    | RepositionTooltip
    | HovercardMsg Hovercard.Msg
    | CloseTooltip (Maybe String) (TooltipType Msg)


type SettingsMsg
    = UserChangedPreferredCurrency String
    | UserToggledValueDisplay


type RequestLimit
    = Unlimited
    | Limited { remaining : Int, limit : Int, reset : Int }


showResetCounterAtRemaining : Int
showResetCounterAtRemaining =
    20


type alias UserModel =
    { auth : Auth
    , apiKey : String
    , hovercard : Maybe Hovercard.Model
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
    | NavBackEffect
    | GetElementEffect { id : String, msg : Result Browser.Dom.Error Browser.Dom.Element -> Msg }
    | GetContentsElementEffect
    | LocaleEffect Effect.Locale.Effect
    | SearchEffect Effect.Search.Effect
    | GraphEffect Effect.Graph.Effect
    | PathfinderEffect Effect.Pathfinder.Effect
    | ApiEffect (Effect.Api.Effect Msg)
    | PluginEffect (Cmd Plugin.Msg)
    | PortsConsoleEffect String
    | CmdEffect (Cmd Msg)
    | LogoutEffect
    | SetDirtyEffect
    | SetCleanEffect
    | SaveUserSettingsEffect UserSettings
    | NotificationEffect Model.Notification.Effect
    | PostponeUpdateByUrlEffect Url


type Thing
    = Entity Api.Data.Entity


userSettingsFromMainModel : Model key -> UserSettings
userSettingsFromMainModel model =
    { selectedLanguage = model.config.locale.locale
    , lightMode = Just model.config.lightmode
    , valueDetail = Just model.config.locale.valueDetail
    , preferredFiatCurrency = Just model.config.preferredFiatCurrency
    , showValuesInFiat = Just model.config.showValuesInFiat
    , addressLabel = Just model.graph.config.addressLabelType
    , edgeLabel = Just model.graph.config.txLabelType
    , showAddressShadowLinks = Just model.graph.config.showAddressShadowLinks
    , showClusterShadowLinks = Just model.graph.config.showEntityShadowLinks
    , showDatesInUserLocale = Just model.config.showDatesInUserLocale
    , showZeroValueTxs = Just model.graph.config.showZeroTransactions
    , showTimeZoneOffset = Just model.config.showTimeZoneOffset
    , showTimestampOnTxEdge = Just model.config.showTimestampOnTxEdge
    , highlightClusterFriends = Just model.config.highlightClusterFriends
    , snapToGrid = Just model.config.snapToGrid
    }
