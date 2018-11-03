# elm-haskell-state-diagram early alpha notes
Each app is written in two main phases, and then iterated on as it is changed:
1. Define the client and server states (including data types) and transitions
2. Implement the generated function stubs for state transitions and view functions

The code can then be compiled and run.
This library will generate much of the client (Elm) and server (Haskell) code for you, including handling state and server 
threads, encoding and decoding messages and ensuring that the code matches the specification directly. This ensures that
you do less boring, tedious, error-prone work and do more of the fun work including writing the state transitions themselves
and the view functions.

## Writing and generating your new app
1. Write the server specification in `ClientServerSpec.hs`, make sure to change the output directory.
It's best if your output directory is outside of this repository (create a new git repo and use `../` to 
output the generated code to the new repo. See below section for notes on the specification.
2. In a terminal window, compile the code generator by typing `stack build`.
3. Run `stack exec elm-haskell-state-diagram-exe` to generate the code in the specified directory.

## ClientServerSpec.hs notes
- You must keep `csDiagram`, `ssDiagram` and `clientServerApp` in the spec (don't edit their names).
- Do not edit `clientConnect` and `clientDisconnect` or their names
- Each server state must be able to handle `ClientConnect` and a `ClientDisconnect` (your implementation
doesn't actually have to change its state based on these if they're not important to your server).
### ClientServerApp
The specification of the entire client-server in question
```
type ClientServerApp =
    ( String                  --starting state of client
    , String                  --starting state of server
    , ClientStates            --include all possible client states here
    , ServerStates            --include all possible server states here
    , ExtraClientTypes        --include all extra client types used in states or messages
    , ExtraServerTypes        --include all extra server types used in states or messages
    , ClientStateDiagram      --the client state diagram
    , ServerStateDiagram      --the client state diagram
    )
```
### ClientStateDiagram
A dictionary of tuples as follows:
`((currentState :: String, stateTransition :: ClientTransition), (nextState :: String, outgoingMessage :: Maybe ServerTransition))`.
- ClientTransition is basically a constructor of a custom type.
- If outgoingMessage is `Nothing` then no message will be sent to the server on that update function being called.
If it is Just ServerTransition, then a message will be sent from that update transition.
### ServerStateDiagram
A dictionary of tuples as follows:
`((currentState :: String, stateTransition :: ServerTransition), (nextState :: String, outgoingMessage :: OutgoingClientMessage))`.
- ServerTransition is basically a constructor of a custom type.
- The `OutgoingClientMessage` is a recursive type with the following constructors:
  - `ToSender            Constructor`                   --reply back to the client that sent the orignal message
  - `ToAllExceptSender   Constructor`                   --send a message to all clients except the sender (should only 
  really be used with an `AllOf` to send a different message to the sender than to the other clients)
  - `ToSenderAnd         Constructor`                   --reply to sender and a `Set` of other clients
  - `ToAll               Constructor`                   --send a message to all connected clients
  - `OneOf               [OutgoingClientMessage]`       --send one of a list of possible messages
  - `AllOf               [OutgoingClientMessage]`       --send all of a list of possible messages
  - `NoClientMessage`                                   -- do not send a message on this transition
- These messages are implemented in much a similar way in the generated code stubs, with each of one except `ToSender`
  having a variant with `F` (e.g. `ToAllF`) which is given a function to change the message data (but not the type)
  based on which clientId it is being sent to.
  
## Implementing functions in the generated code
- Client and server code are generated in their respective directories
- Only code inside of `userApp` should be edited. Other generated code may be used as reference but changing it will break the system. Make sure to fill in the following files for both client and server:
  - `Init`
  - `View*`
  - `Update*`
- In the case of the client, View and Update files will be generated for each state.
- Type signatures guide you as you code and ensure that your implementation matches the specification.

## Building and running your new app
### Client
1. In terminal, navigate to the `client` directory 
2. Run `make` or `make app` to build the Elm JavaScript files.
3. If you haven't already, run `make launch` to view the file in `elm reactor` (the Elm program will not work if you 
open the `index.html` file directly).
4. To rebuild, re-run `make app` and refresh the browser.
5. You can use `make stop` to stop the elm reactor server when you're done.
### Server
1. In terminal, navigate to the `server` directory.
2. Run `stack build` to build the server.
3. Run `stack exec server-exe` to execute the server.
4. Use `Ctrl+C` (potentially twice) to stop the server.
5. Navigating to `http://localhost:8080` directly in the browser will display the current server state, refresh to update
it.

## Making changes to specification
Currently, making changes to the specification is a somewhat manual process. In the case of the client, new states' files 
will be generated but files already existing will not be overwritten. This section provides some guidelines about how to do 
it properly for the time being:
- Make small, incremental changes, regenerating the code each time.
- After you regenerate the code, add in any new update functions, `Msg` types or changes to the `Model` types.
- Use error messages to determine where and what changes need to be made.
