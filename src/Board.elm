module Board exposing (Board, Square, Multiplier, init, view)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (class)

import Matrix exposing (Matrix)
import Tile exposing (Tile, Status(..))

type Multiplier
  = Letter Int
  | Word Int

type Square
  = Empty
  | Occupied Status Tile
  | Premium Multiplier

type Board = B (Matrix Square)

size : Int
size = 15

doubleLetters : List (Int, Int)
doubleLetters =
  [ (0, 3), (0, 11)
  , (2, 6), (2, 8)
  , (3, 0), (3, 7), (3, 14)
  , (6, 2), (6, 6), (6, 8), (6, 12)
  , (7, 3), (7, 11)
  , (8, 2), (8, 6), (8, 8), (8, 12)
  , (11, 0), (11, 7), (11, 14)
  , (12, 6), (12, 8)
  , (14, 3), (14, 11)
  ]

tripleLetters : List (Int, Int)
tripleLetters =
  [ (1, 5), (1, 9)
  , (5, 1), (5, 5), (5, 9), (5, 13)
  , (9, 1), (9, 5), (9, 9), (9, 13)
  , (13, 5), (13, 9)
  ]

doubleWords : List (Int, Int)
doubleWords =
  [ (1, 1), (1, 13)
  , (2, 2), (2, 12)
  , (3, 3), (3, 11)
  , (4, 4), (4, 10)
  , (7, 7)
  , (10, 4), (10, 10)
  , (11, 3), (11, 11)
  , (12, 2), (12, 12)
  , (13, 1), (13, 13)
  ]

tripleWords : List (Int, Int)
tripleWords =
  [ (0, 0), (0, 7), (0, 14)
  , (7, 0), (7, 14)
  , (14, 0), (14, 7), (14, 14)
  ]

setByIndices : List (Int, Int) -> a -> Matrix a -> Matrix a
setByIndices is x matrix = List.foldl (\(i, j) m -> Matrix.set i j x m) matrix is

init : Board
init =
  Empty
    |> Matrix.repeat size size
    |> setByIndices doubleLetters (Premium (Letter 2))
    |> setByIndices tripleLetters (Premium (Letter 3))
    |> setByIndices doubleWords (Premium (Word 2))
    |> setByIndices tripleWords (Premium (Word 3))
    |> B

viewEmpty : Html msg
viewEmpty =
  Html.div
  [ class "empty" ]
  []

viewPremium : Multiplier -> Html msg
viewPremium mult =
  let
    (num, kind, str) =
      case mult of
        Letter i -> (String.fromInt i, "letter", "x LS")
        Word i   -> (String.fromInt i, "word", "x WS")
  in
    Html.div
    [ class ("premium-" ++ kind ++ "-" ++ num) ]
    [ Html.text (num ++ str) ]

viewSquare : msg -> Square -> Html msg
viewSquare event square =
  case square of
    Empty  ->
      viewEmpty
    Occupied status tile ->
      Tile.view event status tile
    Premium mult ->
      viewPremium mult

view : msg -> Board -> Html msg
view event (B matrix) =
  Html.div
    [ class "board" ]
    (matrix
      |> Matrix.toLists
      |> List.concat
      |> List.map (viewSquare event))