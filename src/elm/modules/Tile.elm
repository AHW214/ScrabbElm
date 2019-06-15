module Tile
  exposing
    ( Tile
    , Event
    , blank
    , blankFrom
    , blankToLetter
    , char
    , decoder
    , encoder
    , isBlank
    , letter
    , score
    , string
    , view
    )

import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random exposing (Generator)
import Random.List


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

blankToLetter : Char -> Tile
blankToLetter c =
  Letter c 0

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

view : Tile -> Html msg
view tile =
  let
    viewLetter c v =
      ( []
      , [ Html.div [ HA.class "letter" ] [ Html.text (String.fromChar c) ]
        , Html.div [ HA.class "points" ] [ Html.text (String.fromInt v) ]
        ]
      )

    (attrs, html) =
      case tile of
        Letter c v ->
           viewLetter c v
        Blank (Just c) ->
          viewLetter c 0
        _ ->
          ( [], [] )
    in
      Html.div
        (HA.class "tile" :: attrs)
        html