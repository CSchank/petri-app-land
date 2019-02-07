module Config exposing(..)

-- sets the server URL of the WebSocket server.
-- for servers secured with SSL, use wss://.
-- "ws://localhost:8080" is the default server address
-- for running the server locally.
serverUrl : String
serverUrl = "ws://localhost:8080"