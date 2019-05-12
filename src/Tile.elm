module Tile exposing (Tile, Status(..), blank, letter, view)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

type Status
  = Held
  | Placed

type Tile
  = Blank
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
    check points

blank : Tile
blank = Blank

letter : Char -> Tile
letter c = Letter c (value c)

view : msg -> Status -> Tile -> Html msg
view event status tile =
  let
    html =
      case tile of
        Blank -> []
        Letter c i ->
          [ Html.div [ class "letter" ] [ Html.text (String.fromChar c) ]
          , Html.div [ class "points" ] [ Html.text (String.fromInt i) ]
          ]
    sts =
      case status of
        Placed -> "placed"
        Held   -> "held"
  in
    Html.div
      [ class "tile", class sts, onClick event ]
      html