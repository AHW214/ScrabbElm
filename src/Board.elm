module Board exposing (Board, init, set, placePending, view)

import Dict exposing (Dict)
import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

import Matrix exposing (Matrix)
import Tile exposing (Tile)

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

type Board =
  B Pending (Matrix Cell)

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

-- refactor probably
set : Int -> Int -> Maybe (Int, Tile) -> Board -> (Board, Maybe Int)
set i j held (B pending matrix) =
  case Matrix.get i j matrix of
    Nothing ->
      (B pending matrix, Nothing)
    Just (C kind state) ->
      case (held, state) of
        (Nothing, Pending tile) ->
          ( B (Dict.remove (i, j) pending) (Matrix.set i j (C kind Empty) matrix)
          , Dict.get (i, j) pending |> Maybe.andThen Just
          )
        (Just (r, tile), Pending _) ->
          ( B (Dict.insert (i, j) r pending) (Matrix.set i j (C kind (Pending tile)) matrix)
          , Dict.get (i, j) pending |> Maybe.andThen Just
          )
        (Just (r, tile), Empty) ->
          ( B (Dict.insert (i, j) r pending) (Matrix.set i j (C kind (Pending tile)) matrix)
          , Nothing
          )
        _ -> (B pending matrix, Nothing)


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


type alias Properties msg
  = (List (Attribute msg), List (Html msg))

viewNormal : msg -> Properties msg
viewNormal event =
    ( [ class "empty", onClick event ]
    , []
    )

viewPremium : msg ->  Multiplier -> Properties msg
viewPremium event mult =
  let
    (num, kind, str) =
      case mult of
        Letter n -> (String.fromInt n, "letter", "x LS")
        Word n   -> (String.fromInt n, "word", "x WS")
  in
      ( [ class ("premium-" ++ kind ++ "-" ++ num), onClick event ]
      , [ Html.text (num ++ str) ]
      )

viewPending : msg -> Tile -> Properties msg
viewPending event tile =
    ( [ class "pending", onClick event ]
    , [ Tile.view tile ]
    )

viewPlaced : Tile -> Properties msg
viewPlaced tile =
    ( []
    , [ Tile.view tile ]
    )

viewCell : msg -> List (Attribute msg) -> Cell -> Html msg
viewCell event attr1 (C kind state) =
  let
    (attr2, html) =
      case (kind, state) of
        (Normal, Empty) ->
          viewNormal event
        (Premium mult, Empty) ->
          viewPremium event mult
        (_, Pending tile) ->
          viewPending event tile
        (_, Placed tile) ->
          viewPlaced tile
  in
    Html.div
    (attr1 ++ attr2)
    html

view : (Int -> Int -> msg) -> Maybe a -> Board -> Html msg
view event held (B pending matrix) =
  let
    attr =
      case held of
        Nothing ->
          []
        Just _ ->
          [ class "hover" ]
  in
    Html.div
      [ class "board" ]
      (matrix
        |> Matrix.indexedMap (\i j -> viewCell (event i j) attr)
        |> Matrix.toLists
        |> List.concat)