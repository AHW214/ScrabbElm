module Multiplayer exposing
  ( Event(..), eventDecoder
  , exchangeEncoder, placeEncoder
  , passEncoder, endGameEncoder
  )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

import Tile exposing (Tile)
import Board

type alias Index
  = (Int, Int)

type alias Bag
  = List Tile

type alias Placed
  = List (Index, Tile)

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
  = StartGame Bag
  | Passed
  | Exchanged Bag
  | Placed Bag Placed
  | EndGame

eventDecoder : Decoder Event
eventDecoder =
  Decode.field "eventType" Decode.string
    |> Decode.andThen
      (\event ->
        case event of
          "startGame" ->
            Decode.map StartGame
              (Decode.at [ "data", "bag" ] bagDecoder)

          "exchanged" ->
            Decode.map Exchanged
              (Decode.at [ "data", "bag" ] bagDecoder)

          "placed" ->
            Decode.map2 Placed
              (Decode.at [ "data", "bag" ] bagDecoder)
              (Decode.at [ "data", "placed" ] placedDecoder)

          "passed" ->
            Decode.succeed Passed

          "endGame" ->
            Decode.succeed EndGame

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

placeEncoder : Bag -> Placed -> Value
placeEncoder bag placed =
  eventEncoder "placed"
    (Encode.object
      [ ( "bag", bagEncoder bag )
      , ( "placed", placedEncoder placed )
      ]
    )

passEncoder : Value
passEncoder =
  eventEncoder "pass" Encode.null

endGameEncoder : Value
endGameEncoder =
  eventEncoder "endGame" Encode.null