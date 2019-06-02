-- Based on https://github.com/MattCheely/elm-port-examples/blob/master/websocket

port module WebSocket exposing (ConnectionInfo, Ticket, Event(..), connect, sendString, sendJson, events)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Ticket =
  String

type alias ConnectionInfo =
  { protocol : String
  , extensions : String
  , url : String
  }

connect : String -> List String -> Cmd msg
connect url protocols =
  message "connect"
    (Encode.object
      [ ( "url", Encode.string url )
      , ( "protocols", Encode.list Encode.string protocols )
      ]
    )
    |> toSocket

sendString : ConnectionInfo -> String -> Cmd msg
sendString connection =
  sendJson connection << Encode.string

sendJson : ConnectionInfo -> Value -> Cmd msg
sendJson connection value =
  message "sendString"
    (Encode.object
      [ ( "url", Encode.string connection.url )
      , ( "message", value )
      ]
    )
    |> toSocket


type Event
  = Connected ConnectionInfo
  | StringMessage ConnectionInfo String
  | Closed ConnectionInfo Int
  | Error ConnectionInfo Int
  | BadMessage String

events : (Event -> msg) -> Sub msg
events msg =
  fromSocket
    (\val ->
      case Decode.decodeValue eventDecoder val of
        Ok event ->
          msg event
        Err decodeErr ->
          msg (BadMessage (Decode.errorToString decodeErr))
    )

eventDecoder : Decoder Event
eventDecoder =
  Decode.field "msgType" Decode.string
    |> Decode.andThen
        (\msgType ->
            case msgType of
              "connected" ->
                Decode.map Connected
                  (Decode.field "msg" connectionDecoder)

              "stringMessage" ->
                Decode.map2 StringMessage
                  (Decode.field "msg" connectionDecoder)
                  (Decode.at [ "msg", "data" ] Decode.string)

              "closed" ->
                Decode.map2 Closed
                  (Decode.field "msg" connectionDecoder)
                  (Decode.at [ "msg", "unsentBytes" ] Decode.int)

              "error" ->
                Decode.map2 Error
                (Decode.field "msg" connectionDecoder)
                (Decode.at [ "msg", "code" ] Decode.int)

              _ ->
                Decode.succeed (BadMessage ("Unknown message type: " ++ msgType))
        )

connectionDecoder : Decoder ConnectionInfo
connectionDecoder =
  Decode.map3 ConnectionInfo
    (Decode.field "protocol" Decode.string)
    (Decode.field "extensions" Decode.string)
    (Decode.field "url" Decode.string)

message : String -> Value -> Value
message msgType msg =
  Encode.object
    [ ( "msgType", Encode.string msgType )
    , ( "msg", msg )
    ]

port toSocket : Value -> Cmd msg

port fromSocket : (Value -> a) -> Sub a