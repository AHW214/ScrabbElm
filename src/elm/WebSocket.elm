-- Based on https://github.com/MattCheely/elm-port-examples/blob/master/websocket

port module WebSocket exposing
  ( ConnectionInfo, Ticket, Event(..)
  , connect, sendString
  , sendJsonString, events
  )

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

disconnect : String -> Cmd msg
disconnect url =
  message "connect"
    (Encode.object
      [ ( "url", Encode.string url ) ]
    )
    |> toSocket

sendString : ConnectionInfo -> String -> Cmd msg
sendString connection text =
  message "sendString"
    (Encode.object
      [ ( "url", Encode.string connection.url )
      , ( "message", Encode.string text )
      ]
    )
    |> toSocket

sendJsonString : ConnectionInfo -> Value -> Cmd msg
sendJsonString connection =
  sendString connection << Encode.encode 0


type Event
  = Connected ConnectionInfo
  | StringMessage ConnectionInfo String
  | Closed ConnectionInfo Int (Maybe String)
  | Error ConnectionInfo
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
                Decode.map3 Closed
                  (Decode.field "msg" connectionDecoder)
                  (Decode.at [ "msg", "unsentBytes" ] Decode.int)
                  (Decode.at [ "msg", "reason" ] (Decode.nullable Decode.string))

              "error" ->
                Decode.map Error
                (Decode.field "msg" connectionDecoder)

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