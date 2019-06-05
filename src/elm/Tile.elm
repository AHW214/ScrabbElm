module Tile exposing
  ( Tile, Event, blank, blankFrom, letter
  , isBlank, score, char, string, exchange
  , encoder, decoder, view
  )

import Random.List
import Random exposing (Generator)
import Html exposing (Html, Attribute)
import Html.Events exposing (on, onClick, keyCode)
import Html.Attributes exposing (class, type_)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Event msg
  = Maybe Char -> msg

type Tile
  = Blank (Maybe Char)
  | Letter Char Int

points : List (Int, List Char)
points =
  [ ( 1,  [ 'E', 'A', 'I', 'O', 'N', 'R', 'T', 'L', 'S', 'U' ] )
  , ( 2,  [ 'D', 'G' ] )
  , ( 3,  [ 'B', 'C', 'M', 'P' ] )
  , ( 4,  [ 'F', 'H', 'V', 'W', 'Y' ] )
  , ( 5,  [ 'K' ] )
  , ( 8,  [ 'J', 'X' ] )
  , ( 10, [ 'Q', 'Z' ] )
  ]

pointsLower : List (Int, List Char)
pointsLower =
  [ ( 1,  [ 'e', 'a', 'i', 'o', 'n', 'r', 't', 'l', 's', 'u' ] )
  , ( 2,  [ 'd', 'g' ] )
  , ( 3,  [ 'b', 'c', 'm', 'p' ] )
  , ( 4,  [ 'f', 'h', 'v', 'w', 'y' ] )
  , ( 5,  [ 'k' ] )
  , ( 8,  [ 'j', 'x' ] )
  , ( 10, [ 'q', 'z' ] )
  ]

-- to avoid redundant case expression i guess
value : Char -> Int
value =
  let
    check map c =
      case map of
        [] -> 0
        (p, cs)::rest ->
          if List.member c cs then
            p
          else
            check rest c
  in
    check pointsLower

blank : Tile
blank =
  Blank Nothing

blankFrom : Maybe Char -> Tile
blankFrom mc =
  Blank mc

letter : Char -> Tile
letter c =
  Letter c (value c)

isBlank : Tile -> Bool
isBlank tile =
  case tile of
    Blank _ ->
      True
    _ ->
      False

score : Tile -> Int
score tile =
  case tile of
    Letter _ v -> v
    _          -> 0

char : Tile -> Char
char tile =
  case tile of
  Letter c _ ->
    c
  _ ->
    Debug.todo "Tile.char: Impossible"

string : Tile -> String
string =
  String.fromChar << char

chooseRandom : Int -> (List Tile, List Tile) -> Generator (Maybe (List Tile, List Tile))
chooseRandom i (chosen, bag) =
  if i <= 0 then
    Random.constant (Just (chosen, bag))
  else
    Random.List.choose bag
      |> Random.andThen
        (\(maybeTile, newBag) ->
          case maybeTile of
            Nothing ->
              Random.constant Nothing
            Just tile ->
              Random.lazy (\_ -> chooseRandom (i - 1) (tile :: chosen, newBag))
        )

exchange : List Tile -> List Tile -> Generator (Maybe (List Tile, List Tile))
exchange discarded bag =
  chooseRandom (List.length discarded) ([], bag)
    |> Random.andThen
      (\result ->
        case result of
          Nothing ->
            Random.constant Nothing
          Just (chosen, newBag) ->
            Random.map2
              (\x y -> Just (x, y))
              (Random.constant chosen)
              (Random.List.shuffle (discarded ++ newBag))
      )

decoder : Decoder Tile
decoder =
  Decode.index 0 Decode.int
    |> Decode.andThen (\code -> Decode.index 1 Decode.int
    |> Decode.andThen (\val -> Decode.succeed <|
        case Char.fromCode code of
          ' ' ->
            Blank Nothing
          c ->
            Letter c val
    ))

encoder : Tile -> Value
encoder tile =
  Encode.list Encode.int <|
    case tile of
      Blank _ ->
        [ Char.toCode ' ', 0 ]
      Letter c v ->
        [ Char.toCode c, v ]

view : Maybe (Event msg) -> Tile -> Html msg
view maybeEv tile =
  let
    (attrs, html) =
      case tile of
        Letter c v ->
            ( []
            , [ Html.div [ class "letter" ] [ Html.text (String.fromChar c) ]
              , Html.div [ class "points" ] [ Html.text (String.fromInt v) ]
              ]
            )
        Blank maybeChar ->
          case maybeChar of
            Nothing ->
              ( []
              , []
              )
            Just c ->
              ( []
              , [ Html.text (String.fromChar c) ]
              )
    in
      Html.div
        (class "tile" :: attrs)
        html