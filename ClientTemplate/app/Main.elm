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

import Html exposing(Html)
import Html.Events exposing(onClick)
import Static.Init as Init
import Static.Update
import Static.Encode exposing(encodeTransition)
import Static.Decode exposing(decodeIncomingMessage)
import Static.Version as V
import Static.View
import Static.Types
import Static.Subs
import Static.Types exposing(NetModel)

import Config exposing(serverUrl)
import Utils.Utils exposing(newMsg)

import Bootstrap.Modal as Modal



port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


subscriptions : InternalModel -> Sub Msg
subscriptions model =
    Sub.batch [subPort WSProcess, Sub.map OutgoingTrans <| Static.Subs.subscriptions model.appModel]


getCmdPort : InternalModel -> (Value -> Cmd Msg)
getCmdPort model =
    cmdPort


type alias FunnelState =
    { socket : WebSocket.State }



-- MODEL

type State =
      Connected 
    | NotConnected 
    | ConnectionClosed


defaultUrl : String
defaultUrl =
    serverUrl

type alias Alert =
    {
        name : String
    ,   body : List (Html Msg)
    }

type alias InternalModel =
    { connectionState : State
    , alert : Maybe Alert
    , log : List String
    , url : String
    , wasLoaded : Bool
    , state : FunnelState
    , key : String
    , error : Maybe String
    , appModel: NetModel
    }


main =
    B.application
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
    { connectionState = NotConnected
    , alert = Nothing
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
                    |> addCmd (Cmd.map OutgoingTrans <| Tuple.second Init.init)


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
    | OutgoingTrans Static.Types.NetTransition
    | IncomingMsg Static.Types.NetIncomingMessage
    | NoOp



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
        IncomingMsg incomingMsg -> 
            let 
                (newAppModel, mCmd) = Static.Update.update () incomingMsg model.appModel 
            in
                ({ model | appModel = newAppModel }, Cmd.map (always NoOp) mCmd)
        OutgoingTrans trans ->
            let
                respTxt = encodeTransition trans
                newTrans = Static.Update.outgoingToIncoming trans
            in
                case (respTxt,newTrans) of
                    (Just str, Nothing) -> model |> wsSend str
                    (Nothing, Just nt) -> model |> withCmd (Cmd.map IncomingMsg <| newMsg nt)
                    _ -> model |> withNoCmd
        NoOp -> model |> withNoCmd
        {-OutgoingTrans outgoingTrans ->
            case Static.Update.transitionType outgoingTrans of
                OutgoingToServer -> 
                    let
                        respTxt = encodeOutgoingTransition outgoingTrans
                    in
                        model |> wsSend respTxt
                LocalOnly ->
                    let
                        cmd = case (Static.Update.outgoingToIncoming outgoingTrans) of 
                                Just m -> newMsg m
                                _ -> Cmd.none
                    in
                    model |> withCmd (Cmd.map IncomingMsg cmd)-}

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
            case Debug.log "message" message of 
                "resetfadsfjewi" -> 
                    { model | appModel = Tuple.first Init.init } |> withNoCmd
                "s" -> --server is asking for version
                    model |> wsSend V.version
                "v" -> --correct version
                    { model | appModel = Tuple.first Init.init, connectionState = Connected }
                        |> withNoCmd
                "i" -> --incorrect version
                    let
                        fState = model.state
                    in
                    { model | connectionState = ConnectionClosed
                            , alert = Just <| 
                                { name = "Well, this is embarassing.... :("
                                , body = [Html.text <| "Client-server version mismatch. If you are a user, please contact the creator of the app. Client and server versions must match in order to communicate. If you are a server admin, fix this by making sure the newest client and server versions are compiled. You might have to refresh this page.", Html.div [] [Html.text <| "(client version: " ++ V.version ++ ")"]]
                                }
                            , state = { fState | socket = fState.socket |> WebSocket.setAutoReopen model.key False }
                            }
                        |> withCmd (newMsg WSClose) 
                _ ->
                    let
                        rincomingMsg = decodeIncomingMessage message model.appModel
                        newCmd = 
                            case (Debug.log "decoded message: " rincomingMsg) of 
                                Ok incomingMsg -> Task.perform IncomingMsg (Task.succeed incomingMsg)
                                Err _ -> Cmd.none
                    in            
                        ({ model | log = ("Received \"" ++ message ++ "\"") :: model.log }
                        , newCmd
                        )
                   

        WebSocket.ConnectedResponse _ ->
            { model | log = "Connected" :: model.log }
                |> withNoCmd
                

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model
                | log =
                    ("Closed, " ++ closedString code wasClean expected)
                        :: model.log
                , connectionState = ConnectionClosed
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

view : InternalModel -> B.Document Msg
view model =
    { title = Static.View.title model.appModel
    , body = (case model.connectionState of 
                NotConnected ->         [Html.text "Connecting to server....", Html.button [onClick WSConnect] [Html.text "Attempt Reconnection"]]        
                ConnectionClosed ->     [Html.text "Lost connection. Reconnecting....", Html.button [onClick WSConnect] [Html.text "Attempt Reconnection"]]        
                _ -> [Html.map OutgoingTrans <| Static.View.view model.appModel]) ++ [alert model]
             --   , text <| "Log: " ++ Debug.toString model.log   
    }

alert model = 
    case model.alert of
        Just { name, body } ->
            Modal.config NoOp
                |> Modal.h4 [] [ Html.text name ]
                |> Modal.body [] body
                |> Modal.view Modal.shown
        Nothing ->
            Html.div [] []