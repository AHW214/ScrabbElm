module Board exposing
  ( Board, init, set, isEmpty
  , getTileAt, placePending
  , placeAtIndices, view
  , pendingTilesWordCheck
  , pendingTilesCheckFirstTurn
  )

import Dict exposing (Dict)
import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, class)
import RedBlackTree exposing (Tree)

import Matrix exposing (Matrix)
import Tile exposing (Tile)

type alias Event msg
  = Int -> Int -> msg

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

type alias Index =
  (Int, Int)

type alias Pending =
  Dict Index Int

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
      C Normal (Placed tile)
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

placePending : Board -> (Board, List (Index, Tile))
placePending (B pending matrix) =
  let
    placer =
      Dict.foldl
        (\(i, j) _ (m, p) ->
          case Matrix.get i j m of
            Nothing ->
              (m, p)
            Just (C kind state) ->
              let
                t =
                  case getTile state of
                    Just tile ->
                      tile
                    Nothing ->
                      Debug.todo "placePending: Impossible"
              in
                ( Matrix.set i j (place (C kind state)) m
                , ((i, j), t) :: p
                )
        )
  in
    pending
      |> placer (matrix, [])
      |> Tuple.mapFirst (B Dict.empty)

placeAtIndices : List (Index, Tile) -> Board -> Board
placeAtIndices pairs (B pending matrix) =
  let
    placer =
      List.foldl (\((i, j), tile) m -> Matrix.set i j (C Normal (Placed tile)) m)
  in
    B pending (placer matrix pairs)

getTileAt : Int -> Int -> Board -> Maybe Tile
getTileAt i j (B _ mat) =
  case Matrix.get i j mat of
    Just (C _ state) ->
      getTile state
    _ ->
      Debug.todo "Should not happen"

isEmpty : Board -> Bool
isEmpty (B _ matrix) =
  let
    halfSize = size // 2
  in
    case Matrix.get halfSize halfSize matrix of
      Just (C _ (Placed _)) ->
        False
      _ ->
        True


type alias Properties msg
  = (List (Attribute msg), List (Html msg))

viewNormal : List (Html.Attribute msg) -> Properties msg
viewNormal attrs =
    ( class "empty" :: attrs
    , []
    )

viewPremium : List (Html.Attribute msg) ->  Multiplier -> Properties msg
viewPremium attrs mult =
  let
    (num, kind, str) =
      case mult of
        Letter n ->
          (String.fromInt n, "letter", "x LS")
        Word n ->
          (String.fromInt n, "word", "x WS")
  in
      ( class ("premium-" ++ kind ++ "-" ++ num) :: attrs
      , [ Html.text (num ++ str) ]
      )

viewPending : List (Html.Attribute msg) -> Tile -> Properties msg
viewPending attrs tile =
    ( class "pending" :: attrs
    , [ Tile.view tile ]
    )

viewPlaced : Tile -> Properties msg
viewPlaced tile =
    ( []
    , [ Tile.view tile ]
    )

viewCell : List (Attribute msg) -> Cell -> Html msg
viewCell attrs (C kind state) =
  let
    (attr, html) =
      case (kind, state) of
        (Normal, Empty) ->
          viewNormal attrs
        (Premium mult, Empty) ->
          viewPremium attrs mult
        (_, Pending tile) ->
          viewPending attrs tile
        (_, Placed tile) ->
          viewPlaced tile
  in
    Html.div
      attr
      html

view : Maybe (Event msg) -> Maybe a -> Board -> Html msg
view maybeEv held (B pending matrix) =
  let
    hover =
      case held of
        Nothing ->
          []
        Just _ ->
          [ class "hover" ]

    handlers i j =
      case maybeEv of
        Nothing ->
          []
        Just event ->
          [ onClick (event i j) ]
  in
    Html.div
      [ id "board", class "no-select" ]
      (matrix
        |> Matrix.indexedMap (\i j -> viewCell (hover ++ handlers i j))
        |> Matrix.toLists
        |> List.concat)

------------------------------------Troy's Valid Move Checker --------------------------------------

--direction the placed letters go
--Row is the first entry and Column is the second entry
--in Matrix indexing
type Direction
  = Row
  | Col

getDir : Index -> Index -> Maybe Direction
getDir (i1, j1) (i2, j2) =
  if i1 == i2 then
    Just Row
  else if j1 == j2 then
    Just Col
  else
    Nothing

perpDir : Direction -> Direction
perpDir dir =
  case dir of
    Row -> Col
    Col -> Row

stepIndex : (Int -> Bool) -> (Int -> Int) -> Direction -> Index -> Maybe Index
stepIndex bound op dir (i, j) =
  case dir of
    Row ->
      if bound j then
        Just (i, op j)
      else
        Nothing
    Col ->
      if bound i then
        Just (op i, j)
      else
        Nothing

nextIndex : Direction -> Index -> Maybe Index
nextIndex =
  stepIndex ((>) (size - 1)) ((+) 1)

prevIndex : Direction -> Index -> Maybe Index
prevIndex =
  stepIndex ((<) 0) ((+) -1)

getTile : State -> Maybe Tile
getTile state =
  case state of
    Empty ->
      Nothing
    Pending tile ->
      Just tile
    Placed tile ->
      Just tile

---------------------------------------------------Placing Tiles on the Board as Pending ------------------------------------
{-
--Decides whether it is ok to place a tile at the index (i,j) in the board
okPlace : Index -> Board -> Bool
okPlace index b =
  isValidTilePlacement index b && isCellEmpty index b
-}

--Decides if the index (i,j) in the board is empty
isCellEmpty : Index -> Board -> Bool
isCellEmpty (i, j) (B _ mat) =
  case Matrix.get i j mat of
    Just (C _ Empty) ->
      True
    _ ->
      False

{-
--Checks if placing the tile at index (i,j) is valid:
--I.e. if the tile is in the same row or column as
--the already pending tiles
isValidTilePlacement : Index -> Board -> Bool
isValidTilePlacement (i, j) (B pend _) =
  let
    --Easier to make the dict into a list
    pendList = Dict.keys pend
    (inRow, inCol) =
      List.foldl (\(i2, j2) (r, c) -> (i == i2 && r, j == j2 && c)) (True, True) pendList
  in
    inRow || inCol

--Adds a given tile to the board at (i,j) as a 'Pending' tile
--If it is not possible to add the tile, then we return nothing
addTileToBoard : Index -> Int -> Tile -> Board -> Board
addTileToBoard (i, j) rackIndex tile (B pend mat) =
  if okPlace (i, j) (B pend mat) then
    --update the pending list and also the matrix
    case Matrix.get i j mat of
      Nothing ->
        B pend mat
      Just (C kind _) ->
        B (Dict.insert (i, j) rackIndex pend) (Matrix.set i j (C kind (Pending tile)) mat)
  else
    B pend mat
  -}


--------------------------------------------Check Pending Tiles For Validity -----------------------------------------
{-If the pending tiles are contiguous and the words created by the pending tiles are valid,
  a maybe int is returned. Otherwise, Nothing is returned (i.e. the player's move is invalid).
 I.e., if at least one world is invalid, then nothing is returned.
 Also, if the number of pending tiles is 1 or less, then return nothing
  since 1 or fewer letters does not constitute a valid word.
 Finally, if the pending tiles combined with the permanent tiles
  are not contiguous, then return nothing.
 THIS IS THE FUNCTION TO USE WHEN CHECKING IF A PLAY IS LEGAL-}
pendingTilesWordCheck : Tree String -> Board -> Maybe Int
pendingTilesWordCheck dict board =
  let
    --ordered from least to greatest. Note that because of
    --how a valid "pend" is built, the indices will
    --have either the same column or row. So
    --we are essentially comparing the indices that
    --vary. Ex. (0,3) < (0,4).
    (B pend mat) = board
    orderedPendList = List.sort (Dict.keys pend)
  in
    case orderedPendList of
      [] ->
        --Changed to Just 0 from Nothing as it more accurately models a "no move"
        Just 0
      index::[] ->
        --check both directions
        let
          firstRow = Debug.log "firstr" <| findFirstLetter index Row board
          firstCol = Debug.log "firstc" <| findFirstLetter index Col board
          (valRow, wordRow, multRow) = Debug.log "wordr" <| calculateWord firstRow Row board
          (valCol, wordCol, multCol) = Debug.log "wordc" <| calculateWord firstCol Col board
          checkRow = RedBlackTree.member wordRow dict
          checkCol = RedBlackTree.member wordCol dict
        in
          if String.length wordCol == 1 && checkRow then
            Just (valRow * multRow)
          else if String.length wordRow == 1 && checkCol then
            Just (valCol * multCol)
          else if checkRow && checkCol then
            Just (valRow * multRow + valCol * multCol)
          else
            Nothing
      {-
        in
          if isWordr && isWordc then
            Just (valr*multr + valc*multc)
          else if isWordr && (not isWordc) then
            --Need to check if the column word is of length 1
            if String.length wordc <= 1 then
              Just (valr*multr)
            else
              Nothing
          else if (not isWordr) && isWordc then
            --need to check if the row word is of length 1
            if String.length wordr <= 1 then
              Just (valc*multc)
            else
              Nothing
          else
            Nothing
      -}
      index1::index2::_ ->
          getDir index1 index2
            |> Maybe.andThen (\dir ->
              let
                firstIndex =
                  Debug.log "first" <| findFirstLetter index1 dir board
              in
                if Debug.log "cont" <| isPendContiguous firstIndex dir orderedPendList board && isPendAdjacent orderedPendList mat then
                  let
                    (val1, word, mult) =
                      Debug.log "word" <| calculateWord firstIndex dir board
                    traversed =
                      Debug.log "traverse" <| traversePlayedWord orderedPendList dir dict board
                  in
                    case traversed of
                      Nothing ->
                        Nothing
                      Just val2 ->
                        if RedBlackTree.member word dict then
                          Just (mult * val1 + val2)
                        else
                          Nothing
                else
                  Nothing
              )


pendingTilesCheckFirstTurn : Tree String -> Board -> Maybe Int
pendingTilesCheckFirstTurn dict board =
  let
    --ordered from least to greatest. Note that because of
    --how a valid "pend" is built, the indices will
    --have either the same column or row. So
    --we are essentially comparing the indices that
    --vary. Ex. (0,3) < (0,4).
    (B pend _) = board
    orderedPendList = List.sort (Dict.keys pend)
  in
    case orderedPendList of
      [] ->
        --Changed to Just 0 from Nothing as it more accurately models a "no move"
        Just 0
      _::[] ->
        Nothing
      index1::index2::_ ->
        getDir index1 index2
          |> Maybe.andThen (\dir ->
            let
              firstIndex =
                Debug.log "first" <| findFirstLetter index1 dir board
            in
              if Debug.log "cont" <| isPendContiguous firstIndex dir orderedPendList board && isPendCentered orderedPendList then
                let
                  (val, word, mult) = calculateWord firstIndex dir board
                in
                  if RedBlackTree.member word dict then
                    Just (val * mult)
                  else
                    Nothing
              else
                Nothing
            )

--Checks if any of the pending tiles are in the center cell
isPendCentered : List Index -> Bool
isPendCentered =
  List.any (\(i, j) -> i == 7 && j == 7)

--NOTE: ONLY CALL THE FUNCTIONS BELOW AMONG EACH OTHER OR IN THE THIRD
--CASE STATEMENT OF pendingTilesWordCheck

--************Only use this function when traversing the pending tiles
--This is because the pending tiles are already in order.
--Given an index with a tile (pending or permanent) in it
--find the index of the first letter in the direction specified
--Note that the pending letters must for a contiguous
--"word" with permanent letters allowed to bridge groups of pending letter
findFirstLetter : Index -> Direction -> Board -> Index
findFirstLetter index dir board =
  case prevIndex dir index of
    Nothing ->
      index
    Just prev ->
      if isCellEmpty prev board then
        index
      else
        findFirstLetter prev dir board


--slightly inefficient but oh well. The words are so small that
--we can consider this O(1) time.
--Given a pending tile set, takes in the index (fi, fj) of the first letter of the string
--and then moves to the right or down. If a blank tile is reached
--but the pending list has not been entirely hit, then we know tha
--the played word is not contiguous. Returns True if contiguous and False otherwise.
isPendContiguous : Index -> Direction -> List Index -> Board -> Bool
isPendContiguous index dir pendList board =
  case pendList of
    [] ->
      True
    p::ps ->
      if isCellEmpty index board then
        -- we know that there's a discontinuity, but the pending list has not been consumed
        False
      else
        case Debug.log "next" <| nextIndex dir index of
          Nothing ->
            True
          Just next ->
            let rest = if p == index then ps else pendList in
            isPendContiguous next dir rest board

--Checks if any of the pending tiles are adjacent to a permanent cell. If yes, then return True. If not, return false.
isPendAdjacent : List Index -> Matrix Cell -> Bool
isPendAdjacent pendList mat =
  case pendList of
    [] ->
      False
    (i,j)::ps ->
      let
        lCell = Matrix.get (i-1) j mat
        rCell = Matrix.get (i+1) j mat
        tCell = Matrix.get i (j-1) mat
        dCell = Matrix.get i (j+1) mat
      in
        case (lCell, (rCell, tCell, dCell)) of
          (Just (C _ (Placed _)),( _, _, _)) -> True
          (_, (Just (C _ (Placed _)), _, _)) -> True
          (_, (_, Just (C _ (Placed _)), _)) -> True
          (_, (_, _, Just (C _ (Placed _)))) -> True
          _ -> isPendAdjacent ps mat

--Given a starting position (fi, fj), goes left or down until a blank tile/the board boundary
--is reached. Calculates the Just score if the word is valid, otherwise, return Nothing for the score.
--Output is the score (not multiplied by word multipliers yet), the String, and the word multiplier
--Note, letter multiplying is handled during traversal
calculateWord : Index -> Direction -> Board -> (Int, String, Int)
calculateWord (fi, fj) dir (B pend mat) =
  case stepIndex ((>) size) ((+) 1) dir (fi, fj) of
    Nothing ->
      (0, "", 1)
    Just next ->
      case Matrix.get fi fj mat of
        Just (C kind state) ->
          case getTile state of
            Nothing ->
              (0, "", 1)
            Just tile ->
              let
                (currVal, currString, currMult) =
                  case kind of
                    Normal ->
                      (Tile.score tile, Tile.string tile, 1)
                    Premium (Letter mult) ->
                      (mult * Tile.score tile, Tile.string tile, 1)
                    Premium (Word mult) ->
                      (Tile.score tile, Tile.string tile, mult)
                (recurVal, recurString, recurMult) =
                  calculateWord next dir (B pend mat)
              in
                (recurVal + currVal, currString ++ recurString , recurMult * currMult)
        Nothing ->
          Debug.todo "calculateWord: Should not happen"
    --The tuple from recursive calls

--Traverses the Pending characters and checks if the
--perpendicular words are valid. Returns Just total_score of the perpendicular words are all valid. Nothing otherwise.
traversePlayedWord : List (Int, Int) -> Direction -> Tree String -> Board -> Maybe Int
traversePlayedWord pendList dir dict board =
  case pendList of
    [] ->
      Just 0
    index::pendRest ->
      let
        perp = perpDir dir
        pIndex = findFirstLetter index perp board
      in
        case (traversePlayedWord pendRest dir dict board, calculateWord pIndex perp board) of
          (Nothing, _) ->
            Nothing
          (Just val1, (val2, word, mult)) ->
            if String.length word <= 1 then
              Just val1
            else if RedBlackTree.member word dict then
              Just (val1 + mult * val2)
            else
              Nothing