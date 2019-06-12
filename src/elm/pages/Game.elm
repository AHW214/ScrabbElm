module Page.Game
  exposing
    ( Model
    , Msg
    , init
    , update
    , subscriptions
    , view
    )


-- imports

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode
import Random

import Multiplayer
import RedBlackTree exposing (Tree)
import WebSocket exposing (ConnectionInfo)

import Board exposing (Board)
import Player exposing (Player)
import Rack exposing (Rack)
import Room exposing (Room)
import Tile exposing (Tile)


-- model

type alias Model =
  { room : Room
  , board : Board
  , rack : Rack
  , bag : List Tile
  , held : Maybe ( Int, Tile )
  , turnScore : Maybe Int
  , consecutivePasses : Int
  , state : State
  }

type State
  = Waiting
  | Active
  | Over


init : Room -> ( Model, Cmd Msg )
init room =
  ( { room = room
    , board = Board.init
    , rack = Rack.empty
    , bag = []
    , held = Nothing
    , turnScore = Just 0
    , consecutivePasses = 0
    , state = Waiting
    }
  , WebSocket.connect ("ws://" ++ Multiplayer.serverIP) []
  )


-- update

type Msg
  = ChoseRackPlace Int
  | ChoseRackExchange Int
  | ClickedBoard Int Int
  | SetBlank (Maybe Char)
  | StartExchange
  | EndExchange (Maybe ( List Tile, List Tile ))
  | PassTurn
  | EndTurn
  | StartGame
  | EndGame
  | ReturnToLobby

update : Tree -> Msg -> Model -> ( Model, Cmd Msg )
update dict msg model =
  case msg of
    ReceivedString stringified ->
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

    SocketError errMsg ->
      let
        _ = Debug.log "websocket error: " errMsg
      in
        ( model
        , Cmd.none
        )

    ChoseRackPlace i ->
      let
        ( taken, newRack ) =
          case model.held of
            Nothing ->
              Rack.take i model.rack
            Just ( j, _ ) ->
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
            | rack = newRack
            , held = taken
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
        ( newBoard, maybeRet ) =
          Board.set i j model.held model.board

        validator =
          if Board.isEmpty model.board then
            Board.pendingTilesCheckFirstTurn
          else
            Board.pendingTilesWordCheck

        newTurnScore =
          validator dict newBoard

        newRack =
          case maybeRet of
            Nothing ->
              model.rack
            Just index ->
              Rack.return index model.rack
      in
        ( { model
            | board = newBoard
            , rack = newRack
            , held = Nothing
            , turnScore = newTurnScore
          }
        , Cmd.none
        )

    SetBlank mc ->
      let
        newModel =
          case ( model.held, mc ) of
            ( Just ( index, tile ), char ) ->
              { model
                | rack = Rack.updateBlank index mc model.rack
                , held = Just ( index, Tile.blankToLetter char )
              }
            _ ->
              model
      in
        ( newModel
        , Cmd.none
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
            Just ( exTiles, exBag ) ->
              { model
                | rack = Rack.exchange exTiles model.rack
                , bag = exBag
                , consecutivePasses = model.consecutivePasses + 1
              }
            _ ->
              model
      in
        ( newModel
        , Multiplayer.exchangeEncoder exBag
            |> sendToServer model.socketStatus
        )

    PassTurn ->
      ( { model | consecutivePasses = model.consecutivePasses + 1 }
      , Multiplayer.passEncoder
          |> sendToServer model.socketStatus
      )

    EndTurn ->
      let
        ( newRack, newBag ) =
          Rack.replenish model.bag model.rack

        ( newBoard, placed ) =
          Board.placePending model.board

        turnScore =
          case mode.turnScore of
            Nothing ->
              0
            Just Score ->
              if List.length placed == Rack.size then
                score + 50
              else
                score

        newScore =
          turnScore + Room.myScore model.room

        ( newState, valueOut ) =
          if Rack.allEmpty newRack then
            ( Over
            , Multiplayer.endGameEncoder newScore
            )
          else
            ( Active
            , Multiplayer.placeEncoder newBag placed newScore
            )
      in
        ( { model
            | board = newBoard
            , rack = newRack
            , bag = newBag
            , turnScore = Just 0
            , room = Room.updateMyScore newScore
            , consecutivePasses = 0
            , state = newState
          }
        , sendToServer model.socketStatus valueOut
        )

    StartGame ->
      ( { model | state = Active }
      , Multiplayer.startGameEncoder
          |> sendToServer model.socketStatus
      )

    EndGame ->
      ( { model | state = Over }
      , Multiplayer.endGameEncoder Room.me model.room
          |> sendToServer model.socketStatus
      )

    ReturnToLobby ->
      ( model
      , Cmd.none
      )

handleMultiplayer : Multiplayer.Event -> Model
handleMultiplayer event =
  case event of
    Multiplayer.PlayerJoined player ->
      { model | Room.join player model.room }

    Multiplayer.PlayerLeft player ->
      { model | Room.leave player model.room }

    Multiplayer.StartGame bag ->
      let
        ( newRack, newBag ) = Rack.init bag
      in
        { model
          | room = Room.reset model.room
          , board = Board.init
          , rack = newRack
          , bag = newBag
          , held = Nothing
          , turnScore = Just 0
          , consecutivePasses = 0
          , state = Active
        }

      Multiplayer.Exchanged bag ->
        { model
          | bag = newBag
          , consecutivePasses = model.consecutivePasses + 1
        }

      Multiplayer.Placed newBag placed player ->
        { model
          | bag = newBag
          , board = Board.placeAtIndices placed model.board
          , room = Room.updatePlayer player model.room
          , consecutivePasses = 0
        }

      Multiplayer.Passed ->
        { model | consecutivePasses = model.consecutivePasses + 1 }

      Multiplayer.EndGame player ->
        { model
          | room = Room.updatePlayer player model.room
          , state = Over
        }

sendToServer : SocketStatus -> Encode.Value -> Cmd Msg
sendToServer socketStatus value =
  case socketStatus of
    Connected info ->
      WebSocket.sendJsonString info value
    _ ->
      let
        _ = Debug.log "sendToServer error: socket status is" socketStatus
      in
        Cmd.none


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ WebSocket.events webSocketHandler
    , if isSettingBlank model.held then
        Decode.map SetBlank keyDecoder
          |> Browser.Events.onKeyDown
      else
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

keyDecoder : Decode.Decoder (Maybe Char)
keyDecoder =
  Decode.map toLetter (Decode.field "key" Decode.string)

toLetter : String -> Maybe Char
toLetter str =
  case String.uncons str of
    Just ( c, "" ) ->
      if Char.isAlpha c then
        Just (Char.toLower c)
      else
        Nothing
    _ ->
      Nothing

isSettingBlank : Maybe ( Int, Tile ) -> Bool
isSettingBlank =
  Maybe.withDefault False << Maybe.map (Tile.isBlank << Tuple.second)


-- view

view : Model -> Html Msg
view model =
  case model.state of
    Waiting ->
      viewWaiting model
    Active ->
      viewActive model
    Over ->
      viewOver model

viewOver : Model -> Html Msg
viewOver model =
  Html.div
  []
  [ Html.text "TODO: results screen also buttons to stay or go to lobby" ]

viewWaiting : Model -> Html Msg
viewWaiting =
  viewMain Nothing Nothing

viewActive : Model -> Html Msg
viewActive model =
  let
    defaultRackEvs =
      { placeEv = ChoseRackPlace
      , exchangeEv = ChoseRackExchange
      }

    ( boardEv, rackEvs ) =
      if not (Room.myTurn model.room) then
        ( Nothing, Nothing )
      else if isSettingBlank model.held then
        ( Nothing, Just defaultRackEvs )
      else
        ( Just ClickedBoard, Just defaultRackEvs )
  in
    viewMain boardEv rackEvs model

viewMain : Maybe Board.Event -> Maybe Rack.Events -> Model -> Html Msg
viewMain boardEv rackEvs { room, rack, held } =
  Html.div
    [ HA.id "wrapper" ]
    [ Html.div
      [ HA.class "centered" ]
      [ Html.div
          [ HA.style "display" "flex" ]
          [ Board.view boardEv held
          , Html.div
            [ HA.class "sidebar" ]
            [ viewGameInfo room ]
          ]
        , Rack.view rackEvs rack
      ]
    ]

viewGameInfo : Room -> Html Msg
viewGameInfo room =
  Html.div
    [ HA.id "info" ]
    (List.concatMap room.players
      (\p ->
        [ Player.view p
        , if Room.theirTurn p room then
            viewTimer 0
          else
            viewTimer 0
        ]
      )
    )

viewTimer : Int -> Html Msg
viewTimer time =
  Html.div
    [ class "time" ]
    [ Html.text <| String.fromInt time ]