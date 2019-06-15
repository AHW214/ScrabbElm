module Page.Connecting
  exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )


-- imports

import Html exposing (Html)
import Html.Attributes as HA
import Http
import Json.Decode as Decode exposing (Decoder)

import WebSocket exposing (ConnectionInfo, Ticket)


-- model

type Model
  = Init
  | Requested Ticket
  | Authorizing Ticket ConnectionInfo
  | Authorized ConnectionInfo


init : ( Model, Cmd Msg )
init =
  ( Init
  , Http.get
      { url = ("http://" ++ serverIP)
      , expect = Http.expectString ReceivedTicket
      }
  )

serverIP : String
serverIP =
  "18.191.239.101:3000"


-- update

type Msg
  = ReceivedTicket (Result Http.Error String)
  | ServerConnected ConnectionInfo
  | ServerMessage String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case ( msg, model ) of
    ( ReceivedTicket result, Init ) ->
      case result of
        Err errMsg ->
          Debug.todo "go to disconnected page"
        Ok ticket ->
          ( Requested ticket
          , WebSocket.connect ("ws://" ++ serverIP) []
          )

    ( ServerConnected connInfo, Requested ticket ) ->
      ( Authorizing ticket connInfo
      , WebSocket.sendString connInfo ticket
      )

    ( ServerMessage message, Authorizing _ connInfo ) ->
      let
        newModel =
          case Decode.decodeString authDecoder message of
            Err errMsg ->
              let
                _ = Debug.log "connecting server message decode error: " errMsg
              in
                model

            Ok authorized ->
              if authorized then
                Debug.todo "go to lobby page"
              else
                Debug.todo "go to disconnected page"
      in
        ( newModel
        , Cmd.none
        )

    ( _, _ ) ->
      ( model
      , Cmd.none
      )


authDecoder : Decoder Bool
authDecoder =
  Decode.map ((==) "authorized")
    Decode.string


-- view

view : Model -> Html Msg
view model =
  case model of
    Init ->
      Html.div [] [ Html.text "contacting server..." ]

    Requested _ ->
      Html.div [] [ Html.text "connecting..." ]

    Authorizing _ _ ->
      Html.div [] [ Html.text "authorizing..." ]

    Authorized _ ->
      Html.div [] [ Html.text "connected!" ]