module Board exposing (Board, Square, Multiplier, init, view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Matrix exposing (Matrix)

type Multiplier
  = Letter Int
  | Word Int

type Square
  = Blank
  | Tile Char
  | Premium Multiplier

type Board = B (Matrix Square)

doubleLetters : List (Int, Int)
doubleLetters = [ (0, 3), (0, 11)
                , (2, 6), (2, 8)
                , (3, 0), (3, 7), (3, 14)
                , (6, 2), (6, 6), (6, 8), (6, 12)
                , (7, 3), (7, 11)
                , (8, 2), (8, 6), (8, 8), (8, 12)
                , (11, 0), (11, 7), (11, 14)
                , (12, 6), (12, 8)
                , (14, 3), (14, 11)
                ]

setByIndices : List (Int, Int) -> a -> Matrix a -> Matrix a
setByIndices is x matrix = List.foldl (\(i, j) m -> Matrix.set i j x m) matrix is

init : Board
init =
  Blank
    |> Matrix.repeat 15 15
    |> setByIndices doubleLetters (Premium (Letter 2))
    |> B

className : Square -> String
className square =
  case square of
    Blank  -> "blank"
    Tile _ -> "tile"
    Premium mult ->
      case mult of
        Letter i -> "premium-letter-" ++ String.fromInt i
        Word i   -> "premium-word-" ++ String.fromInt i

viewSquare : Square -> Html msg
viewSquare square =
  let
    (text, name) =
      case square of
        Blank  -> ([], "blank")
        Tile c -> ([String.fromChar c], "tile")
        Premium mult ->
          case mult of
            Letter i ->
              let num = String.fromInt i in
              ([num ++ "x LS"], "premium-letter-" ++ num)
            Word i   ->
              let num = String.fromInt i in
              ([num ++ "x WS"], "premium-word-" ++ num)
  in
    Html.div
      [ class "square", class name]
      (List.map Html.text text)

view : Board -> Html msg
view (B matrix) =
  Html.div
    [ class "board" ]
    (matrix
      |> Matrix.toLists
      |> List.concat
      |> List.map viewSquare)