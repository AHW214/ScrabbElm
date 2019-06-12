module Main
  exposing
    (..)


-- imports

import Browser
import Html exposing (Html)
import Http

import RedBlackTree as RBT exposing (Tree)
import WebSocket exposing (ConnectionInfo, Ticket)

import Page.Connecting as Connecting
import Page.Disconnected as Disconnected
import Page.Game as Game
import Page.Lobby as Lobby


-- program

type alias Flags = ()

main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


-- model

type alias Model =
  { socketInfo : SocketStatus
  , dict : Tree String
  , page : Page
  }

type Page
  = Connecting Connecting.Model
  | Disconnected Disconnected.Model
  | Lobby Lobby.Model
  | Game Game.Model


type SocketStatus
  = Unopened
  | Connected ConnectionInfo
  | Closed (Maybe String)


init : Flags -> ( Model, Cmd Msg )
init () =
  ( { socketInfo = Unopened
    , dict = RBT.empty
    , page = Connecting Connecting.init
    }
  , Cmd.batch
      [ Http.get
          { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
          , expect = Http.expectString GotDict
          }
      , Http.get
          { url = ("http://" ++ Multiplayer.serverIP)
          , expect = Http.expectString GotTicket
          }
      ]
  )


-- update

type Msg
  = SocketConnect ConnectionInfo
  | SocketClosed (Maybe String)
  | ReceivedString String
  | SocketError String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case ( msg, model ) of
    ( SocketClosed reason, _ ) ->
      ( { model
          | socketInfo = Closed reason
          , page = Disconnected Disconnected.init
        }
      , Cmd.none
      )

    ( SocketError errMsg, _ ) ->
      let
        _ = Debug.log "websocket error: " errMsg
      in
        ( model
        , Cmd.none
        )

    ( SocketConnect info, Connecting _ ) ->
      ( { model | socketInfo = Connected info }
      , WebSocket.sendString info model.room.id
      )

    ( ReceivedString stringified, _ ) ->
      let
        newModel =
          case Decode.decodeString Multiplayer.eventDecoder stringified of
            Err errMsg ->
              let
                _ = Debug.log "multiplayer decode error: " errMsg
              in
                model

            Ok event ->
              handleMultiplayer event
      in
        ( newModel
        , Cmd.none
        )

    


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Lobby lobby ->
      Sub.map LobbyMsg (Lobby.subscriptions lobby)

    Game game ->
      Sub.map GameMsg (Game.subscriptions game)

    _ ->
      Sub.none