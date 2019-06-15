module Page.Game
  exposing
    ( Model
    , Msg(..)
    , init
    , update
    , subscriptions
    , view
    )


-- imports

import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random

import RedBlackTree exposing (Tree)
import WebSocket exposing (ConnectionInfo)

import Bag exposing (Bag)
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
  , Cmd.none
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
  | ServerMessage String

update : ConnectionInfo -> Tree -> Msg -> Model -> ( Model, Cmd Msg )
update connInfo dict msg model =
  case msg of
    ServerMessage stringified ->
      let
        newModel =
          case Decode.decodeString eventDecoder stringified of
            Err errMsg ->
              let
                _ = Debug.log "game server message decode error: " errMsg
              in
                model

            Ok event ->
              handleServerEvent event model
      in
        ( newModel
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
        , exchangeEncoder exBag
            |> WebSocket.sendJsonString connInfo
        )

    PassTurn ->
      ( { model | consecutivePasses = model.consecutivePasses + 1 }
      , WebSocket.sendJsonString connInfo passEncoder
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
            , endGameEncoder newScore
            )
          else
            ( Active
            , placeEncoder newBag placed newScore
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
        , WebSocket.sendJsonString connInfo valueOut
        )

    StartGame ->
      ( { model | state = Active }
      , WebSocket.sendJsonString connInfo startGameEncoder
      )

    EndGame ->
      ( { model | state = Over }
      , endGameEncoder Room.me model.room
          |> WebSocket.sendJsonString connInfo
      )

    ReturnToLobby ->
      ( model
      , Cmd.none
      )

handleServerEvent : WsEvent -> Model -> Model
handleServerEvent event model =
  case event of
    PlayerJoined player ->
      { model | Room.join player model.room }

    PlayerLeft player ->
      { model | Room.leave player model.room }

    GameStarted bag ->
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

      Passed ->
        { model | consecutivePasses = model.consecutivePasses + 1 }

      Exchanged bag ->
        { model
          | bag = newBag
          , consecutivePasses = model.consecutivePasses + 1
        }

      Placed newBag placed player ->
        { model
          | bag = newBag
          , board = Board.placeAtIndices placed model.board
          , room = Room.updatePlayer player model.room
          , consecutivePasses = 0
        }

      GameEnded player ->
        { model
          | room = Room.updatePlayer player model.room
          , state = Over
        }


-- websocket events

type WsEvent
  = PlayerJoined Player
  | PlayerLeft Player
  | GameStarted Bag
  | Passed
  | Exchanged Bag
  | Placed Bag Placed Player
  | GameEnded Player

type alias Index
  = ( Int, Int )

type alias Placed
  = List ( Index, Tile )

indexDecoder : Decoder Index
indexDecoder =
  Decode.map2 Tuple.pair
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.int)

indexEncoder : Index -> Value
indexEncoder ( i, j ) =
  Encode.list Encode.int [ i, j ]

placedDecoder : Decoder Placed
placedDecoder =
  Decode.list <|
    Decode.map2 Tuple.pair
      (Decode.field "index" indexDecoder)
      (Decode.field "tile" Tile.decoder)

placedEncoder : Placed -> Value
placedEncoder =
  Encode.list
    (\( index, tile ) ->
      Encode.object
        [ ( "index", indexEncoder index )
        , ( "tile", Tile.encoder tile )
        ]
    )

eventDecoder : Decoder WsEvent
eventDecoder =
  Decode.field "eventType" Decode.string
    |> Decode.andThen
        (\event ->
            case event of
              "playerJoined" ->
                Decode.map PlayerJoined
                  (Decode.at [ "data", "player" ] Player.decoder)

              "playerLeft" ->
                Decode.map PlayerLeft
                  (Decode.at [ "data", "player" ] Player.decoder)

              "gameStarted" ->
                Decode.map GameStarted
                  (Decode.at [ "data", "bag" ] Bag.decoder)

              "passed" ->
                Decode.succeed Passed

              "exchanged" ->
                Decode.map Exchanged
                  (Decode.at [ "data", "bag" ] Bag.decoder)

              "placed" ->
                Decode.map3 Placed
                  (Decode.at [ "data", "bag" ] Bag.decoder)
                  (Decode.at [ "data", "placed" ] placedDecoder)
                  (Decode.at [ "data", "player" ] Player.decoder)

              "gameEnded" ->
                Decode.map GameEnded
                  (Decode.at [ "data", "player" ] Player.decoder)

              _ ->
                Decode.fail "unknown websocket event"
        )

eventEncoder : String -> Value -> Value
eventEncoder eventType data =
  Encode.object
    [ ( "eventType", Encode.string eventType )
    , ( "data", data )
    ]

startGameEncoder : Value
startGameEncoder =
  eventEncoder "gameStarted" Encode.null

passEncoder : Value
passEncoder =
  eventEncoder "passed" Encode.null

exchangeEncoder : Bag -> Value
exchangeEncoder bag =
  eventEncoder "exchanged" <|
    Encode.object
      [ ( "bag", Bag.encoder bag ) ]

placeEncoder : Bag -> Placed -> Int -> Value
placeEncoder bag placed score =
  eventEncoder "placed" <|
    Encode.object
      [ ( "bag", Bag.encoder bag )
      , ( "placed", placedEncoder placed )
      , ( "score", Encode.int score)
      ]

endGameEncoder : Int -> Value
endGameEncoder score =
  eventEncoder "gameEnded" <|
    Encode.object
      [ ( "score", Encode.int score ) ]


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  if isSettingBlank model.held then
    Decode.map SetBlank keyDecoder
      |> Browser.Events.onKeyDown
  else
    Sub.none

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