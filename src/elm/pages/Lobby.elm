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

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode

import WebSocket exposing (ConnectionInfo)