module Bag
  exposing
    ( Bag
    , exchange
    , numBlanks
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random.List
import Random exposing (Generator)

import Tile exposing (Tile)

type alias Bag
  = List Tile

numBlanks : Int
numBlanks = 2

exchange : List Tile -> Bag -> Generator ( List Tile, Bag )
exchange discarded bag =
  chooseRandom (List.length discarded) bag
    |> Random.andThen
      (\( chosen, newBag ) ->
          Random.map2
            Tuple.pair
            (Random.constant chosen)
            (Random.List.shuffle (discarded ++ newBag))
      )

chooseRandom : Int -> Bag -> Generator ( List Tile, Bag )
chooseRandom n =
  let
    helper i chosen bag =
      if i <= 0 then
        Random.constant ( chosen, bag )
      else
        Random.List.choose bag
          |> Random.andThen
            (\( maybeTile, newBag ) ->
              case maybeTile of
                Nothing ->
                  Random.constant (chosen, bag)
                Just tile ->
                  Random.lazy (\_ -> helper (i - 1) (tile :: chosen) newBag)
            )
  in
    helper n []

decoder : Decoder Bag
decoder =
  Decode.list Tile.decoder

encoder : Bag -> Value
encoder =
  Encode.list Tile.encoder