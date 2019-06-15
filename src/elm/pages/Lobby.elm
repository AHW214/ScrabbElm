module Page.Lobby
  exposing
    ( Model
    , Msg(..)
    , init
    , update
    , subscriptions
    , view
    )


-- imports

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

import WebSocket exposing (ConnectionInfo)

import Room exposing (Room)


-- model

type alias Model =
  { rooms : Dict String Room
  , configRoom : Bool
  }

init : ( Model, Cmd Msg )
init =
  ( { rooms = Dict.empty
    , configRoom = False
    }
  , Cmd.none
  )


-- update

type Msg
  = ConfigRoom
  | CloseConfig
  | MakeRoom String Int Bool
  | ServerMessage String

update : ConnectionInfo -> Msg -> Model -> ( Model, Cmd Msg )
update connInfo msg model =
  case msg of
    ServerMessage stringified ->
      let
        newModel =
          case Decode.decodeString eventDecoder stringified of
            Err errMsg ->
              let
                _ = Debug.log "lobby server message decode error: " errMsg
              in
                model

            Ok event ->
              handleServerEvent event
      in
        ( newModel
        , Cmd.none
        )

    ConfigRoom ->
      ( { model | configRoom = True }
      , Cmd.none
      )

    CloseConfig ->
      ( { model | configRoom = False }
      , Cmd.none
      )

    MakeRoom name capacity privacy ->
      ( model
      , Room.new name capacity privacy
          |> makeRoomEncoder
          |> WebSocket.sendJsonString connInfo
      )

handleServerEvent : WsEvent -> Model -> Model
handleServerEvent event model =
  case event of
    RoomDict rooms ->
      { model | rooms = rooms }

    AddRoom room ->
      { model | rooms = Dict.insert room.name room model.rooms }

    RemoveRoom name ->
      { model | rooms = Dict.remove name model.rooms }

    MadeRoom ->
      Debug.todo "transition to game page"


-- websocket events

type WsEvent
  = RoomDict (Dict String Room)
  | AddRoom Room
  | RemoveRoom String
  | MadeRoom

roomsDecoder : Decoder (Dict String Room)
roomsDecoder =
  Decode.map
    (Dict.map Room.fromInfo)
    (Decode.dict Room.infoDecoder)

eventDecoder : Decoder WsEvent
eventDecoder =
  Decode.field "eventType" Decode.string
    |> Decode.andThen
      (\event ->
          case event of
            "roomDict" ->
              Decode.map RoomDict
                (Decode.at [ "data", "rooms" ] roomsDecoder)

            "addRoom" ->
              Decode.map AddRoom
                (Decode.at [ "data", "room" ] Room.decoder)

            "removeRoom" ->
              Decode.map RemoveRoom
                (Decode.at [ "data", "room" ] Decode.string)

            "madeRoom" ->
              Decode.succeed MadeRoom
      )

eventEncoder : String -> Value -> Value
eventEncoder eventType data =
  Encode.object
    [ ( "eventType", Encode.string eventType )
    , ( "data", data )
    ]

makeRoomEncoder : Room -> Value
makeRoomEncoder room =
  eventEncoder "makeRoom" <|
    Encode.object
      [ ( "room", Room.encoder room ) ]