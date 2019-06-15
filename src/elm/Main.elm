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
  { dict : Tree String
  , page : Page
  }

type Page
  = Connecting Connecting.Model
  | Disconnected String Disconnected.Model
  | Lobby ConnectionInfo Lobby.Model
  | Game ConnectionInfo Game.Model


init : Flags -> ( Model, Cmd Msg )
init () =
  ( { socketInfo = Unopened
    , dict = RBT.empty
    , page = Connecting Connecting.init
    }
  , Http.get
      { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
      , expect = Http.expectString GotDict
      }
  )


-- update

type Msg
  = SocketConnect ConnectionInfo
  | SocketClosed (Maybe String)
  | ReceivedString String
  | SocketError String
  | ConnectingMsg Connecting.Msg
  | DisconnectedMsg Disconnected.Msg
  | LobbyMsg Lobby.Msg
  | GameMsg Game.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case ( msg, model.page ) of
    ( SocketConnect connInfo, Connecting connecting ) ->
        Connecting.update (Connecting.ServerConnected connInfo) connecting
          |> updatePage Connecting ConnectingMsg model

    ( SocketClosed reason, _ ) ->
      ( { model , page = Disconnected Disconnected.init }
      , Cmd.none
      )

    ( ReceivedString message, page ) ->
        case page of
          Lobby lobby ->
            Lobby.update (Lobby.ServerMessage message) lobby
              |> updatePage Lobby LobbyMsg model

          Game game ->
            Game.update (Game.ServerMessage message) game
              |> updatePage Game GameMsg model

          Connecting connecting ->
            Connecting.update (Connecting.ServerMessage message) connecting
              |> updatePage Connecting ConnectingMsg model

          _ ->
              ( model
              , Cmd.none
              )

    ( SocketError errMsg, _ ) ->
      let
        _ = Debug.log "websocket error: " errMsg
      in
        ( model
        , Cmd.none
        )

    ( ConnectingMsg pageMsg, Connecting connecting ) ->
      Connecting.update pageMsg connecting
        |> updatePage Connecting ConnectingMsg model

    ( DisconnectedMsg pageMsg, Disconnected disconnected ) ->
      Disconnected.update pageMsg disconnected
        |> updatePage Disconnected DisconnectedMsg model

    ( LobbyMsg pageMsg, Lobby lobby ) ->
      Lobby.update pageMsg lobby
        |> updatePage Lobby LobbyMsg model

    ( GameMsg pageMsg, Game game ) ->
      Game.update pageMsg game
        |> updatePage Lobby GameMsg model

    ( _, _ ) ->
      ( model
      , Cmd.none
      )

updatePage : Page -> (pageMsg -> Msg) -> Model -> (pageModel, Cmd pageMsg) -> ( Model, Cmd Msg )
updatePage page toMsg model ( pageModel, pageMsg ) =
  ( { model | page = page pageModel }
  , Cmd.map toMsg pageMsg
  )


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ WebSocket.events webSocketHandler
    , case model of
        Lobby lobby ->
          Sub.map LobbyMsg (Lobby.subscriptions lobby)

        Game game ->
          Sub.map GameMsg (Game.subscriptions game)

        _ ->
          Sub.none
    ]

webSocketHandler : WebSocket.Event -> Msg
webSocketHandler event =
  case event of
    WebSocket.Connected info ->
      SocketConnect info

    WebSocket.StringMessage _ message ->
      ReceivedString message

    WebSocket.Closed _ _ reason ->
      SocketClosed reason

    WebSocket.Error _ ->
      SocketError "websocket error"

    WebSocket.BadMessage error ->
      SocketError error