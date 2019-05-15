module Board exposing (Board, init, setPending, removePending, placePending, view)


import Dict exposing (Dict)
import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

import Matrix exposing (Matrix)
import Tile exposing (Tile)

type alias Event msg
  = Int -> Int -> msg

type alias Events msg =
  { evEmpty : Event msg
  , evPending : Event msg
  }

type Multiplier
  = Letter Int
  | Word Int

type Kind
  = Normal
  | Premium Multiplier

type State
  = Empty
  | Pending Tile
  | Placed Tile

type Cell
  = C Kind State

type alias Pending =
  Dict (Int, Int) Int

type Board = B Pending (Matrix Cell)

normal : Cell
normal =
  C Normal Empty

premium : Multiplier -> Cell
premium mult =
  C (Premium mult) Empty

place : Cell -> Cell
place (C kind state) =
  case state of
    Pending tile ->
      C kind (Placed tile)
    _ ->
      C kind state

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
  normal
    |> Matrix.repeat size size
    |> setByIndices doubleLetters (premium (Letter 2))
    |> setByIndices tripleLetters (premium (Letter 3))
    |> setByIndices doubleWords (premium (Word 2))
    |> setByIndices tripleWords (premium (Word 3))
    |> B Dict.empty

setPending : Int -> Int -> (Int, Tile) -> Board -> (Board, Maybe Tile)
setPending i j (r, t1) (B pending matrix) =
  case Matrix.get i j matrix of
    Nothing ->
      (B pending matrix, Nothing)
    Just (C kind state) ->
      case state of
        Empty ->
          (B (Dict.insert (i, j) r pending) (Matrix.set i j (C kind (Pending t1)) matrix), Nothing)
        Pending t2 ->
          (B (Dict.insert (i, j) r pending) (Matrix.set i j (C kind (Pending t1)) matrix), Just t2)
        Placed _ ->
          (B pending matrix, Nothing)

removePending : Int -> Int -> Board -> (Board, Maybe Int)
removePending i j (B pending matrix) =
  case Matrix.get i j matrix of
    Nothing ->
      (B pending matrix, Nothing)
    Just (C kind state) ->
      case state of
        Pending tile ->
          ( B (Dict.remove (i, j) pending) (Matrix.set i j (C kind Empty) matrix)
          , Dict.get (i, j) pending |> Maybe.andThen Just)
        _ -> (B pending matrix, Nothing)

-- inefficient ig (maybe store list of indices for pending tiles)
placePending : Board -> Board
placePending (B pending matrix) =
  let
    newMatrix =
      Dict.foldl
        (\(i, j) _ m ->
          case Matrix.get i j m of
            Nothing   -> m
            Just cell ->
              Matrix.set i j (place cell) m
        )
        matrix
        pending
  in
    B Dict.empty newMatrix

viewNormal : msg -> Html msg
viewNormal event =
  Html.div
    [ class "empty", onClick event ]
    []

viewPremium : msg ->  Multiplier -> Html msg
viewPremium event mult =
  let
    (num, kind, str) =
      case mult of
        Letter n -> (String.fromInt n, "letter", "x LS")
        Word n   -> (String.fromInt n, "word", "x WS")
  in
    Html.div
      [ class ("premium-" ++ kind ++ "-" ++ num), onClick event ]
      [ Html.text (num ++ str) ]

viewPending : msg -> Tile -> Html msg
viewPending event tile =
  Html.div
    [ class "pending", onClick event ]
    [ Tile.view tile ]

viewPlaced : Tile -> Html msg
viewPlaced tile =
  Html.div
    []
    [ Tile.view tile ]

viewCell : Events msg -> Int -> Int -> Cell -> Html msg
viewCell { evEmpty, evPending } i j (C kind state) =
  case (kind, state) of
    (Normal, Empty) ->
      viewNormal (evEmpty i j)
    (Premium mult, Empty) ->
      viewPremium (evEmpty i j) mult
    (_, Pending tile) ->
      viewPending (evPending i j) tile
    (_, Placed tile) ->
      viewPlaced tile

view : Events msg -> Board -> Html msg
view events (B pending matrix) =
  Html.div
    [ class "board" ]
    (matrix
      |> Matrix.indexedMap (viewCell events)
      |> Matrix.toLists
      |> List.concat)