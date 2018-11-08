port module Main exposing (main)

{-| WebSocketClient Example
-}

import Browser as B
import Browser.Navigation as Nav
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Json.Decode as D
import PortFunnel exposing (FunnelSpec, GenericMessage, ModuleDesc, StateAccessors)
import PortFunnel.WebSocket as WebSocket
import String
import Url
import Task

import GraphicSVG exposing(..)

import Static.Msg exposing(ClientMessage)
import Static.Model exposing(Model)
import Static.Init as Init
import Static.Update
import Static.Encode exposing(encodeServerMessage)
import Static.Decode exposing(decodeWrappedClientMessage)
import Static.Version as V
import Static.View
import Static.Types
import Static.Subs


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


subscriptions : InternalModel -> Sub Msg
subscriptions model =
    Sub.batch [subPort WSProcess, Sub.map AppMsg <| Static.Subs.subscriptions model.appModel]


getCmdPort : InternalModel -> (Value -> Cmd Msg)
getCmdPort model =
    cmdPort


type alias FunnelState =
    { socket : WebSocket.State }



-- MODEL

type State =
    Connected | NotConnected | ConnectionClosed


defaultUrl : String
defaultUrl =
    "ws://localhost:8080"


type alias InternalModel =
    { appState : State
    , log : List String
    , url : String
    , wasLoaded : Bool
    , state : FunnelState
    , key : String
    , error : Maybe String
    , appModel: Model
    }


main =
    GraphicSVG.app
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = NewUrlRequest
        , onUrlChange = NewUrlChange
        }


initialFunnelState : FunnelState
initialFunnelState =
    { socket = WebSocket.initialState }


init : D.Value -> Url.Url -> Nav.Key -> ( InternalModel, Cmd Msg )
init _ url key =
    { appState = NotConnected
    , log = []
    , url = defaultUrl
    , wasLoaded = False
    , state = initialFunnelState
    , key = "socket"
    , error = Nothing
    , appModel = Tuple.first Init.init
    }
      |> \model -> 
                model |> withCmd
                    (WebSocket.makeOpenWithKey model.key model.url
                        |> send model
                    )
                    |> addCmd (Cmd.map AppMsg <| Tuple.second Init.init)


socketAccessors : StateAccessors FunnelState WebSocket.State
socketAccessors =
    StateAccessors .socket (\substate state -> { state | socket = substate })


type alias AppFunnel substate message response =
    FunnelSpec FunnelState substate message response InternalModel Msg


type Funnel
    = SocketFunnel (AppFunnel WebSocket.State WebSocket.Message WebSocket.Response)


funnels : Dict String Funnel
funnels =
    Dict.fromList
        [ ( WebSocket.moduleName
          , FunnelSpec socketAccessors
                WebSocket.moduleDesc
                WebSocket.commander
                socketHandler
                |> SocketFunnel
          )
        ]



-- UPDATE


type Msg
    = WSConnect
    | WSClose
    | WSSend String
    | WSProcess Value
    | NewUrlRequest B.UrlRequest
    | NewUrlChange Url.Url
    | AppMsg Static.Types.WrappedClientMessage



update : Msg -> InternalModel -> ( InternalModel, Cmd Msg )
update msg model =
    case msg of
        WSConnect ->
            model
                |> withCmd
                    (WebSocket.makeOpenWithKey model.key model.url
                        |> send model
                    )

        WSSend m ->
            { model
                | log =
                    ("Sending \"" ++ m ++ "\"") :: model.log
            }
                |> wsSend m

        WSClose ->
            { model
                | log = "Closing" :: model.log
            }
                |> withCmd
                    (WebSocket.makeClose model.key
                        |> send model
                    )

        WSProcess value ->
            case
                PortFunnel.processValue funnels
                    appTrampoline
                    value
                    model.state
                    model
            of
                Err error ->
                    { model | error = Just error } |> withNoCmd

                Ok res ->
                    res

        NewUrlRequest urlReq -> model |> withNoCmd
        NewUrlChange url -> model |> withNoCmd
        AppMsg appMsg -> 
            let 
                (newAppModel, mCmd, msMsg) = Static.Update.update appMsg model.appModel
            in
                case (mCmd, msMsg) of 
                    (Just cmd, Just sMsg) -> 
                        let
                            respTxt = encodeServerMessage sMsg
                        in
                            { model | appModel = newAppModel } 
                                |> wsSend respTxt 
                                |> addCmd (Cmd.map AppMsg cmd)
                    (Nothing, Just sMsg) -> 
                        let
                            respTxt = encodeServerMessage sMsg
                        in
                            { model | appModel = newAppModel
                            } |> wsSend respTxt
                    (Just cmd, Nothing) -> 
                        { model | appModel = newAppModel } |> withCmd (Cmd.map AppMsg cmd)
                    _ ->
                        { model | appModel = newAppModel } |> withNoCmd

wsSend : String -> InternalModel -> (InternalModel, Cmd Msg)
wsSend m model = 
    withCmd
        (WebSocket.makeSend model.key m
            |> send model
        ) model

appTrampoline : GenericMessage -> Funnel -> FunnelState -> InternalModel -> Result String ( InternalModel, Cmd Msg )
appTrampoline genericMessage funnel state model =
    let
        theCmdPort =
            getCmdPort model
    in
    case funnel of
        SocketFunnel appFunnel ->
            PortFunnel.appProcess theCmdPort
                genericMessage
                appFunnel
                state
                model


send : InternalModel -> WebSocket.Message -> Cmd Msg
send model message =
    WebSocket.send (getCmdPort model) message


doIsLoaded : InternalModel -> InternalModel
doIsLoaded model =
    if not model.wasLoaded && WebSocket.isLoaded model.state.socket then
        { model
            | wasLoaded = True
        }

    else
        model


socketHandler : WebSocket.Response -> FunnelState -> InternalModel -> ( InternalModel, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            doIsLoaded
                { mdl
                    | state = state
                    , error = Nothing
                }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            case message of 
                "resetfadsfjewi" -> 
                    { model | appModel = Tuple.first Init.init } |> withNoCmd
                _ ->
                    let
                        (rincomingMsg,_) = decodeWrappedClientMessage (Err "", String.split "\u{0000}" (Debug.log "Incoming message" message))
                        newCmd = 
                            case rincomingMsg of 
                                Ok incomingMsg -> Task.perform AppMsg (Task.succeed incomingMsg)
                                Err _ -> Cmd.none
                    in            
                        ({ model | log = ("Received \"" ++ message ++ "\"") :: model.log }
                        , newCmd
                        )
                   

        WebSocket.ConnectedResponse _ ->
            { model | log = "Connected" :: model.log, appState = Connected }
                |> (if model.appState == NotConnected then 
                        wsSend V.version
                    else
                        withNoCmd
                   )

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model
                | log =
                    ("Closed, " ++ closedString code wasClean expected)
                        :: model.log
                , appState = ConnectionClosed
            }
                |> withNoCmd

        WebSocket.ErrorResponse error ->
            { model | log = WebSocket.errorToString error :: model.log }
                |> withNoCmd

        _ ->
            model |> withNoCmd


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "NOT expected"
           )

view : InternalModel -> { body : Collage Msg, title: String }
view model =
    { title = Static.View.title model.appModel
    , body = case model.appState of 
                NotConnected ->         collage 500 500 [text "Connecting to server...." |> fixedwidth |> centered |> size 24 |> filled black]        
                ConnectionClosed ->     collage 500 500 [text "Lost connection. Reconnecting...." |> fixedwidth |> centered |> size 24 |> filled black]        
                Connected ->            GraphicSVG.mapCollage AppMsg <| Static.View.view model.appModel
             --   , text <| "Log: " ++ Debug.toString model.log   
    }