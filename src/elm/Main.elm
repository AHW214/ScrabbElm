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
import Player exposing (Player)


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
  | Closed Int (Maybe String)

type State
  = Lobby
  | GameActive
  | GameOver
  | NoConnection String

type alias Model =
  { socketInfo: SocketStatus
  , state : State
  , dict : Tree String
  , board : Board
  , rack : Rack
  , bag : List Tile
  , held : Maybe (Int, Tile)
  , turnScore : Maybe Int
  , myScore : Int
  , myTurn : Bool
  , opponent : Maybe Player
  , consecutivePasses : Int
  }

initModel : Model
initModel =
  { socketInfo = Unopened
  , state = Lobby
  , dict = empty
  , board = Board.init
  , rack = Rack.empty
  , bag = []
  , held = Nothing
  , turnScore = Just 0
  , myScore = 0
  , myTurn = False
  , opponent = Nothing
  , consecutivePasses = 0
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( initModel
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


-- Update

type Msg
  = SocketConnect ConnectionInfo
  | SocketClosed Int (Maybe String)
  | ReceivedString String
  | Error String
  | GotTicket (Result Http.Error String)
  | GotDict (Result Http.Error String)
  | ChoseRackPlace Int
  | ChoseRackExchange Int
  | ClickedBoard Int Int
  | SetBlank (Maybe Char)
  | EndExchange (Maybe (List Tile, List Tile))
  | StartExchange
  | PassTurn
  | EndTurn
  | StartGame
  | EndGame
  | ReturnToLobby

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
              { model | state = NoConnection "Failed to receive ticket from server" }
      in
        ( newModel
        , WebSocket.connect ("ws://" ++ Multiplayer.serverIP) []
        )

    SocketConnect info ->
      ( { model | socketInfo = Connected info }
      , WebSocket.sendString info (getConnectionTicket model.socketInfo)
      )

    SocketClosed code reason ->
      ( { model
          | socketInfo = Closed code reason
          , state = NoConnection (Maybe.withDefault "No connection to server..." reason)
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
                Multiplayer.PlayerJoined player ->
                  { model | opponent = Just player }

                Multiplayer.PlayerLeft player ->
                  { model
                    | state = GameOver
                    , opponent = Nothing
                  }

                Multiplayer.StartGame bag ->
                  let
                    (newRack, newBag) = Rack.init bag
                  in
                    { model
                      | state = GameActive
                      , board = Board.init
                      , rack = newRack
                      , bag = newBag
                      , held = Nothing
                      , turnScore = Just 0
                      , myScore = 0
                      , opponent = Maybe.map (Player.setScore 0) model.opponent
                      , consecutivePasses = 0
                    }

                Multiplayer.Exchanged newBag ->
                  { model
                    | myTurn = True
                    , bag = newBag
                    , consecutivePasses = model.consecutivePasses + 1
                  }

                Multiplayer.Placed newBag placed theirScore ->
                  { model
                    | myTurn = True
                    , bag = newBag
                    , board = Board.placeAtIndices placed model.board
                    , opponent = Maybe.map (Player.setScore theirScore) model.opponent
                    , consecutivePasses = 0
                  }

                Multiplayer.Passed ->
                  { model
                    | myTurn = True
                    , consecutivePasses = model.consecutivePasses + 1 }

                Multiplayer.EndGame theirScore ->
                  { model
                    | state = GameOver
                    , myTurn = False
                    , opponent = Maybe.map (Player.setScore theirScore) model.opponent
                  }
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
          if Board.isEmpty model.board then
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

    SetBlank mc ->
      let
        (newHeld, newRack) =
          case model.held of
            Just (index, tile) ->
              case mc of
                Just c ->
                  ( Just (index, Tile.blankToLetter c)
                  , Rack.updateBlank index mc model.rack
                  )
                Nothing ->
                  ( model.held
                  , model.rack
                  )
            _ ->
              Debug.todo "SetBlank: impossible"
      in
        ( { model
            | held = newHeld
            , rack = newRack
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
          model.myScore + turnScore

        (newState, valueOut) =
          if Rack.allEmpty newRack then
            ( GameOver
            , Multiplayer.endGameEncoder newScore
            )
          else
            ( GameActive
            , Multiplayer.placeEncoder newBag placed newScore
            )
      in
        ( { model
            | state = newState
            , myTurn = False
            , board = newBoard
            , rack = newRack
            , bag = newBag
            , turnScore = Just 0
            , myScore = Debug.log "TOTAL SCORE" newScore
            , consecutivePasses = 0
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
                  , consecutivePasses = model.consecutivePasses + 1
              }
      in
        ( newModel
        , WebSocket.sendJsonString
            (getConnectionInfo model.socketInfo)
            (Multiplayer.exchangeEncoder newModel.bag)
        )

    PassTurn ->
      ( { model
        | myTurn = False
        , consecutivePasses = model.consecutivePasses + 1
        }
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

    EndGame ->
      ( { model
          | state = GameOver
          , myTurn = False
        }
      , WebSocket.sendJsonString
          (getConnectionInfo model.socketInfo)
          (Multiplayer.endGameEncoder model.myScore)
      )

    ReturnToLobby ->
      ( { model
          | state = Lobby
          , myTurn = False
          , myScore = 0
          , opponent = Nothing
        }
      , Cmd.none
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
  let
    blankSubs =
      if isSettingBlank model.held then
        [ Browser.Events.onKeyDown <| Decode.map SetBlank keyDecoder ]
      else
        []

    webSub =
      WebSocket.events
        (\event ->
            case event of
              WebSocket.Connected info ->
                SocketConnect info

              WebSocket.StringMessage info message ->
                ReceivedString message

              WebSocket.Closed _ unsentBytes reason ->
                SocketClosed unsentBytes reason

              WebSocket.Error _ code ->
                Error ("WebSocket Error: " ++ String.fromInt code)

              WebSocket.BadMessage error ->
                Error error
        )

  in
    Sub.batch
      (webSub :: blankSubs)

keyDecoder : Decode.Decoder (Maybe Char)
keyDecoder =
  Decode.map toLetter (Decode.field "key" Decode.string)

toLetter : String -> Maybe Char
toLetter str =
  case String.uncons str of
    Just (c, "") ->
      if Char.isAlpha c then
        Just (Char.toLower c)
      else
        Nothing
    _ ->
      Nothing

-- View

viewTurn : Model -> List (Html Msg)
viewTurn { rack, held, bag, turnScore, myTurn, consecutivePasses } =
  if not myTurn then
    [ Html.button
      []
      [ Html.text "Opponent's turn..." ]
    ]
  else if isSettingBlank held then
    [ Html.button
        []
        [ Html.text "Choose a letter" ]
    ]
  else if Rack.exchanging rack then
    if List.length bag < Rack.size then
      [ Html.button
          []
          [ Html.text "Not enough tiles..." ]
      ]
    else
      [ Html.button
          [ onClick StartExchange ]
          [ Html.text "Exchange Tiles." ]
      ]
  else
    case turnScore of
      Nothing ->
        [ Html.button
            []
            [ Html.text "Finish your move..." ]
        ]
      Just score ->
        if score > 0 || Rack.allChosenBlank rack then
          [ Html.button
              [ onClick EndTurn ]
              [ Html.text "End turn." ]
          ]
        else
          let
            endGame =
              if consecutivePasses >= 6 then
                [ Html.button
                    [ onClick EndGame ]
                    [ Html.text "End game?" ]
                ]
              else
                []
            in
            [ Html.button
                [ onClick PassTurn ]
                [ Html.text "Pass turn." ]
            ] ++ endGame

viewScore : Int -> Int -> Html Msg
viewScore myScore theirScore =
  Html.div
    [ id "score" ]
    [ Html.text <| "My Score: " ++ (String.fromInt myScore)
    , Html.br [] []
    , Html.text <| "Their Score: " ++ (String.fromInt theirScore)
    ]

viewGame : Model -> Html Msg
viewGame model =
  let
    defaultRackEvs =
      { placeEv = ChoseRackPlace, exchangeEv = ChoseRackExchange }

    ( boardEv, rackEvs ) =
      if not model.myTurn then
        ( Nothing, Nothing )
      else if isSettingBlank model.held then
        ( Nothing, Just defaultRackEvs )
      else
        ( Just ClickedBoard, Just defaultRackEvs )
  in
    Html.div
      [ id "wrapper" ]
      [ Html.div
          [ class "centered" ]
          ([ viewScore model.myScore <| Maybe.withDefault 0 <| Maybe.map (.score) model.opponent
          , Board.view boardEv model.held model.board
          , Rack.view rackEvs model.rack
          ] ++ viewTurn model)
      ]

viewLobby : Model -> Html Msg
viewLobby { opponent } =
  let
    html =
      case opponent of
        Nothing ->
          Html.text "Waiting for opponent..."
        _ ->
          Html.button [ onClick StartGame ] [ Html.text "Start Game" ]
  in
    Html.div
      [ id "wrapper" ]
      [ Html.div
          [ id "lobby", class "centered" ]
          [ Html.text "Lobby"
          , Html.br [] []
          , html
          ]
      ]

viewGameOver : Model -> Html Msg
viewGameOver { myScore, opponent } =
  let
    (resultText, (buttonAttr, buttonHtml)) =
      case opponent of
        Just opp ->
          let
            startStr =
              if myScore > opp.score then
                "Congratulations! You won "
              else if opp.score > myScore then
                "Darn, looks like you lost "
              else
                "Woah, you tied "
            in
              ( startStr ++ String.fromInt myScore ++ " to " ++ String.fromInt opp.score ++ "."
              , ( [ onClick StartGame ]
                , [ Html.text "Rematch?" ]
                )
              )
        Nothing ->
          (  "Looks like your opponent left..."
          , ( [ onClick ReturnToLobby ]
            , [ Html.text "Return to lobby." ]
            )
          )
  in
    Html.div
      [ id "wrapper" ]
      [ Html.div
          [ id "resultScreen", class "centered" ]
          [ Html.text resultText
          , Html.button buttonAttr buttonHtml
          ]
      ]

viewNoConnection : String -> Html Msg
viewNoConnection message =
  Html.div
    [ id "wrapper" ]
    [ Html.div
        [ id "noConnection", class "centered" ]
        [ Html.text message ]
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
        NoConnection message ->
          viewNoConnection message
      ]
  }

isSettingBlank : Maybe (Int, Tile) -> Bool
isSettingBlank =
  Maybe.withDefault False << Maybe.map (Tile.isBlank << Tuple.second)