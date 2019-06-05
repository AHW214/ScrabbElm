module Multiplayer exposing
  ( Event(..), eventDecoder
  , exchangeEncoder, placeEncoder
  , passEncoder, endGameEncoder
  , startGameEncoder, serverIP
  )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

import Player exposing (Player)
import Tile exposing (Tile)
import Board

type alias Index
  = (Int, Int)

type alias Bag
  = List Tile

type alias Placed
  = List (Index, Tile)

serverIP : String
serverIP =
  "18.191.239.101:3000"

bagDecoder : Decoder Bag
bagDecoder =
  Decode.list Tile.decoder

bagEncoder : Bag -> Value
bagEncoder =
  Encode.list Tile.encoder

indexDecoder : Decoder Index
indexDecoder =
  Decode.map2 Tuple.pair
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.int)

indexEncoder : Index -> Value
indexEncoder ( i, j ) =
  Encode.list Encode.int [ i, j ]

placedEncoder : Placed -> Value
placedEncoder =
  Encode.list
    (\(index, tile) ->
      Encode.object
        [ ( "index", indexEncoder index )
        , ( "tile", Tile.encoder tile )
        ]
    )

placedDecoder : Decoder Placed
placedDecoder =
  Decode.list <|
    Decode.map2 Tuple.pair
      (Decode.field "index" indexDecoder)
      (Decode.field "tile" Tile.decoder)

type Event
  = PlayerJoined Player
  | PlayerLeft Player
  | StartGame Bag
  | Passed
  | Exchanged Bag
  | Placed Bag Placed Int
  | EndGame Int

eventDecoder : Decoder Event
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

          "startGame" ->
            Decode.map StartGame
              (Decode.at [ "data", "bag" ] bagDecoder)

          "exchanged" ->
            Decode.map Exchanged
              (Decode.at [ "data", "bag" ] bagDecoder)

          "placed" ->
            Decode.map3 Placed
              (Decode.at [ "data", "bag" ] bagDecoder)
              (Decode.at [ "data", "placed" ] placedDecoder)
              (Decode.at [ "data", "score" ] Decode.int)

          "passed" ->
            Decode.succeed Passed

          "endGame" ->
            Decode.map EndGame
              (Decode.at [ "data", "score" ] Decode.int)

          _ ->
            Decode.fail "Unknown server event: "
      )

eventEncoder : String -> Value -> Value
eventEncoder eventType data =
  Encode.object
    [ ( "eventType", Encode.string eventType )
    , ( "data", data )
    ]

exchangeEncoder : Bag -> Value
exchangeEncoder bag =
  eventEncoder "exchanged"
    (Encode.object
      [ ( "bag", bagEncoder bag ) ]
    )

placeEncoder : Bag -> Placed -> Int -> Value
placeEncoder bag placed score =
  eventEncoder "placed"
    (Encode.object
      [ ( "bag", bagEncoder bag )
      , ( "placed", placedEncoder placed )
      , ( "score", Encode.int score )
      ]
    )

passEncoder : Value
passEncoder =
  eventEncoder "passed" Encode.null

endGameEncoder : Int -> Value
endGameEncoder score =
  eventEncoder "endGame"
    (Encode.object
      [ ( "score", Encode.int score ) ]
    )

startGameEncoder : Value
startGameEncoder =
  eventEncoder "startGame" Encode.null