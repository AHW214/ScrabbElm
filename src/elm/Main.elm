module Main exposing (..)

-- Imports

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, class)
import Http
import Random exposing (Generator)
import Random.List
import Json.Decode as Decode
import Json.Encode as Encode

import RedBlackTree exposing (Tree, empty, insert, member)
import WebSocket exposing (ConnectionInfo, Ticket)
import Multiplayer exposing (eventDecoder)
import Board exposing (Board)
import Rack exposing (Rack)
import Tile exposing (Tile)


-- Program

type alias Flags = ()

main : Program Flags Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type SocketStatus
  = Unopened
  | Requested Ticket
  | Connected ConnectionInfo
  | Closed Int

type State
  = Lobby
  | GameActive
  | GameOver
  | NoConnection

type alias Model =
  { socketInfo: SocketStatus
  , state : State
  , dict : Tree String
  , board : Board
  , rack : Rack
  , bag : List Tile
  , held : Maybe (Int, Tile)
  , turnScore : Maybe Int
  , totalScore : Int
  , boardEmpty : Bool
  , myTurn : Bool
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( { socketInfo = Unopened
            , state = Lobby
            , dict = empty
            , board = Board.init
            , rack = Rack.empty
            , bag = List.map Tile.letter (String.toList "historyhornfarmpastemobbitamsndlnfdsl")
            , held = Nothing
            --Changed from Nothing as that more accurately depicts a move without doing anything
            , turnScore = Just 0
            , totalScore = 0
            , boardEmpty = True
            , myTurn = False
            }
          , Cmd.batch
              [ Http.get
                  { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
                  , expect = Http.expectString GotDict
                  }
              , Http.get
                  { url = "http://127.0.0.1:3000"
                  , expect = Http.expectString GotTicket
                  }
              ]
          )


-- Update

type Msg
  = SocketConnect ConnectionInfo
  | SocketClosed Int
  | ReceivedString String
  | Error String
  | GotTicket (Result Http.Error String)
  | GotDict (Result Http.Error String)
  | ChoseRackPlace Int
  | ChoseRackExchange Int
  | ClickedBoard Int Int
  | EndExchange (Maybe (List Tile, List Tile))
  | StartExchange
  | PassTurn
  | EndTurn
  | StartGame

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotTicket result ->
      let
        newModel =
          case result of
            Ok ticket ->
              { model | socketInfo = Requested ticket }
            Err _ ->
              { model | state = NoConnection }
      in
        ( newModel
        , WebSocket.connect "ws://127.0.0.1:3000" []
        )

    SocketConnect info ->
      ( { model | socketInfo = Connected info }
      , WebSocket.sendString info (getConnectionTicket model.socketInfo)
      )

    SocketClosed code ->
      ( { model
          | socketInfo = Closed code
          , state = NoConnection
        }
      , Cmd.none
      )

    ReceivedString eventObject ->
      let
        newModel =
          case Decode.decodeString Multiplayer.eventDecoder eventObject of
            Err errMsg ->
              let
                _ = Debug.log "Multiplayer Error" errMsg
              in
                model

            Ok event ->
              case event of
                Multiplayer.StartGame bag ->
                  let
                    (newRack, newBag) = Rack.init bag
                  in
                    { model
                      | state = GameActive
                      , rack = newRack
                      , bag = newBag
                    }

                Multiplayer.Exchanged newBag ->
                  { model
                    | myTurn = True
                    , bag = newBag
                  }

                Multiplayer.Placed newBag placed boardEmpty ->
                  { model
                    | myTurn = True
                    , bag = newBag
                    , board = Board.placeAtIndices placed model.board
                    , boardEmpty = boardEmpty
                  }

                Multiplayer.Passed ->
                  { model | myTurn = True }

                Multiplayer.EndGame ->
                  { model | state = GameOver }
      in
        ( newModel
        , Cmd.none
        )

    Error errMsg ->
      let
        _ = Debug.log "Error" errMsg
      in
      ( model
      , Cmd.none
      )

    GotDict result ->
      let
        newModel =
          case result of
            Ok text ->
              { model | dict = loadDictionary text }
            Err _ ->
              model
      in
        ( newModel
        , Cmd.none
        )

    ChoseRackPlace i ->
      let
        (taken, newRack) =
          case model.held of
            Nothing ->
              Rack.take i model.rack
            Just (j, _) ->
              if j == i then
                ( Nothing
                , Rack.return i model.rack
                )
              else
                model.rack
                |> Rack.return j
                |> Rack.take i
      in
        ( { model
            | held = taken
            , rack = newRack
          }
        , Cmd.none
        )

    ChoseRackExchange i ->
      let
        newRack =
          Rack.chooseToExchange i model.rack
      in
        ( { model | rack = newRack }
        , Cmd.none
        )

    ClickedBoard i j ->
      let
        (newBoard, maybeRet) =
          Board.set i j model.held model.board

        validator =
          if model.boardEmpty then
            Board.pendingTilesCheckFirstTurn
          else
            Board.pendingTilesWordCheck

        newTurnScore =
          Debug.log "SCORE" <| validator model.dict newBoard

        newRack =
          case maybeRet of
            Nothing ->
              model.rack
            Just index ->
              Rack.return index model.rack
      in
        ( { model
            | board = newBoard
            , held = Nothing
            , rack = newRack
            , turnScore = newTurnScore
          }
        , Cmd.none
        )

    EndTurn ->
      let
        (newRack, newBag) =
          Rack.replenish model.bag model.rack

        (newBoard, placed) =
          Board.placePending model.board

        turnScore =
          case model.turnScore of
            Nothing -> 0
            Just score ->
              if List.length placed == Rack.size then
                score + 50
              else
                score

        newScore =
          model.totalScore + turnScore

        boardEmpty =
          if model.boardEmpty == False then
            False
          else
            case Board.getTileAt 7 7 newBoard of
              Just _ ->
                False
              _ ->
                model.boardEmpty

        (newState, valueOut) =
          if Rack.allEmpty newRack then
            ( GameOver
            , Multiplayer.endGameEncoder
            )
          else
            ( GameActive
            , Multiplayer.placeEncoder newBag placed boardEmpty
            )
      in
        ( { model
            | state = newState
            , myTurn = False
            , board = newBoard
            , rack = newRack
            , bag = newBag
            , turnScore = Just 0
            , totalScore = Debug.log "TOTAL SCORE" newScore
            , boardEmpty = boardEmpty
          }
        , WebSocket.sendJsonString
            (getConnectionInfo model.socketInfo)
            (valueOut)
        )

    StartExchange ->
      let
        tiles =
          Rack.tilesToExchange model.rack
      in
        ( model
        , Random.generate
            EndExchange (Tile.exchange tiles model.bag)
        )

    EndExchange result ->
      let
        newModel =
          case result of
            Nothing ->
              model
            Just (exTiles, exBag) ->
              { model
                  | myTurn = False
                  , rack = Rack.exchange exTiles model.rack
                  , bag = exBag
              }
      in
        ( newModel
        , WebSocket.sendJsonString
            (getConnectionInfo model.socketInfo)
            (Multiplayer.exchangeEncoder newModel.bag)
        )

    PassTurn ->
      ( { model | myTurn = False }
      , WebSocket.sendJsonString
          (getConnectionInfo model.socketInfo)
          (Multiplayer.passEncoder)
      )

    StartGame ->
      ( { model | myTurn = True }
      , WebSocket.sendJsonString
          (getConnectionInfo model.socketInfo)
          (Multiplayer.startGameEncoder)
      )


loadDictionary : String -> Tree String
loadDictionary = RedBlackTree.fromList << String.words

getConnectionTicket : SocketStatus -> Ticket
getConnectionTicket socketInfo =
  case socketInfo of
    Requested ticket ->
      ticket
    _ ->
      Debug.todo "Not connected to server."

getConnectionInfo : SocketStatus -> ConnectionInfo
getConnectionInfo socketInfo =
  case socketInfo of
    Connected info ->
      info
    _ ->
      Debug.todo "Not connected to server."


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.events
    (\event ->
        case event of
          WebSocket.Connected info ->
            SocketConnect info

          WebSocket.StringMessage info message ->
            ReceivedString message

          WebSocket.Closed _ unsentBytes ->
            SocketClosed unsentBytes

          WebSocket.Error _ code ->
            Error ("WebSocket Error: " ++ String.fromInt code)

          WebSocket.BadMessage error ->
            Error error
    )


-- View

viewTurn : Model -> Html Msg
viewTurn { rack, bag, turnScore } =
  let
    (attr, html) =
      if Rack.exchanging rack then
        if List.length bag < Rack.size then
          ([], [ Html.text "Not enough tiles..." ])
        else
          ([ onClick StartExchange ], [ Html.text "Exchange Tiles." ])
      else
        case turnScore of
          Nothing ->
            ([], [ Html.text "Finish your move..." ])
          Just score ->
            if score > 0 then
              ([ onClick EndTurn ], [ Html.text "End turn." ])
            else
              ([ onClick PassTurn ], [ Html.text "Pass turn." ])
  in
    Html.button attr html

viewScore : Int -> Html Msg
viewScore s =
  Html.div [ id "score" ] [ Html.text (String.fromInt s) ]

viewGame : Model -> Html Msg
viewGame model =
  if model.myTurn then
    Html.div
      [ id "wrapper" ]
      [ Html.div
          [ class "centered" ]
          [ Board.view ClickedBoard model.held model.board
          , Rack.view { placeEv = ChoseRackPlace, exchangeEv = ChoseRackExchange } model.rack
          , viewTurn model
          ]
      , viewScore model.totalScore
      ]
  else
    Html.div
      [ id "wrapper" ]
      [ Html.div
        [ class "centered" ]
        [ Html.text "waiting..." ]
      ]

viewLobby : Model -> Html Msg
viewLobby model =
  Html.div
    [ id "wrapper" ]
    [ Html.div
        [ class "centered" ]
        [ Html.text "Lobby"
        , Html.button [ onClick StartGame ] [ Html.text "Start Game" ]
        ]
    ]

viewGameOver : Model -> Html Msg
viewGameOver model =
  Html.div
    [ id "wrapper" ]
    [ Html.div
        [ class "centered" ]
        [ Html.text "Game Over" ]
    ]

viewNoConnection : Model -> Html Msg
viewNoConnection model =
  Html.div
    [ id "wrapper" ]
    [ Html.div
        [ class "centered" ]
        [ Html.text "No connection to server..." ]
    ]

view : Model -> Browser.Document Msg
view model =
  { title = "ScrabbElm"
  , body =
      [ case model.state of
        Lobby ->
          viewLobby model
        GameActive ->
          viewGame model
        GameOver ->
          viewGameOver model
        NoConnection ->
          viewNoConnection model
      ]
  }