module Board exposing (Board, init, set, placePending, view, pendingTilesWordCheck, addTileToBoard)

import Dict exposing (Dict)
import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import RedBlackTree

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

------------------------------------Troy's Valid Move Checker --------------------------------------
--placeholder dictionary.
dict = RedBlackTree.empty

--direction the placed letters go
--Row is the first entry and Column is the second entry
--in Matrix indexing
type Direction = Row | Col
---------------------------------------------------Placing Tiles on the Board as Pending ------------------------------------
--Decides whether it is ok to place a tile at the index (i,j) in the board
okPlace : Int -> Int -> Board -> Bool
okPlace i j b =
    (isValidTilePlacement i j b) && (isCellEmpty i j b)

--Decides if the index (i,j) in the board is empty
isCellEmpty : Int -> Int -> Board -> Bool
isCellEmpty i j (B _ mat) =
  case (Matrix.get i j mat) of
    Just (C _ Empty) -> True
    _ -> False

--Checks if placing the tile at index (i,j) is valid:
--I.e. if the tile is in the same row or column as
--the already pending tiles
isValidTilePlacement : Int -> Int -> Board -> Bool
isValidTilePlacement i j (B pend _) =
  let
    --Easier to make the dirct into a list
    dictList = Dict.keys pend
  in
    case dictList of
      [] -> True
      [(i2,j2)] -> i == i2 || j == j2
      (i2, j2)::(i3,j3)::rest -> (i == i2 && i == i3) || (j == j2 && i == j3)

--Adds a given tile to the board at (i,j) as a 'Pending' tile
--If it is not possible to add the tile, then we return nothing
addTileToBoard : Int -> Int -> Int -> Tile -> Board -> Maybe Board
addTileToBoard i j rackIndex tile (B pend mat) =
  if (okPlace i j (B pend mat)) then
    --update the pending list and also the matrix
    let
      oldCell = Matrix.get i j mat
    in
      (case oldCell of
        Nothing -> Debug.todo "Error: addTileToBoard with bad indices"
        Just (C kind _) ->
          Just (B
                  (Dict.insert (i,j) rackIndex pend)
                  (Matrix.set i j (C kind (Pending tile)) mat)))
  else
    Nothing

--------------------------------------------Check Pending Tiles For Validity -----------------------------------------
{-If the pending tiles are contiguous and the words created by the pending tiles are valid,
  a maybe int is returned. Otherwise, Nothing is returned (i.e. the player's move is invalid).
 I.e., if at least one world is invalid, then nothing is returned.
 Also, if the number of pending tiles is 1 or less, then return nothing
  since 1 or fewer letters does not constitute a valid word.
 Finally, if the pending tiles combined with the permanent tiles
  are not contiguous, then return nothing.
 THIS IS THE FUNCTION TO USE WHEN CHECKING IF A PLAY IS LEGAL-}
pendingTilesWordCheck : Board -> Maybe Int
pendingTilesWordCheck (B pend mat) =
  let
    --ordered from least to greatest. Note that because of
    --how a valid "pend" is built, the indicies will
    --have either the same column or row. So
    --we are essentially comparing the indicices that
    --vary. Ex. (0,3) < (0,4).
    orderedPendList = List.sort (Dict.keys pend)
  in
    case orderedPendList of
      [] -> Nothing
      [t] -> Nothing
      (i1, j1)::(i2, j2)::_ ->
        let
          direction =
            (if i1 == i2 then Row else {-j1 == j2-} Col)
          (fi, fj)= findFirstLetter i1 j1 direction (B pend mat)
        in
          --Placeholder
          if isPendContiguous fi fj direction orderedPendList (B pend mat) && isPendAdjacent orderedPendList mat then
            (case (calculateWord fi fj direction (B pend mat), traversePlayedWord fi fj direction (B pend mat)) of
              (_, Nothing) -> Nothing
              ((val1, word, mult), Just val2) ->
                if RedBlackTree.member word dict then
                  Just (val1*mult + val2)
                else
                  Nothing)
          else
            Nothing

--NOTE: ONLY CALL THE FUNCTIONS BELOW AMONG EACH OTHER OR IN THE THIRD
--CASE STATEMENT OF pendingTilesWordCheck

--************Only use this function when traversing the pending tiles
--This is because the pending tiles are already in order.
--Given an index with a tile (pending or permanent) in it
--find the index of the first letter in the direction specified
--Note that the pending letters must for a contiguous
--"word" with permanent letters allowed to bridge groups of pending letter
findFirstLetter : Int -> Int -> Direction -> Board -> (Int, Int)
findFirstLetter i j direction b =
  if direction == Row then
    if 0 < i && (not (isCellEmpty (i-1) j b)) then
      findFirstLetter (i-1) j direction b
    else
      (i,j)
  else
    if 0 < j && (not (isCellEmpty i (j-1) b)) then
      findFirstLetter i (j-1) direction b
    else
      (i,j)


--slightly inefficient but oh well. The words are so small that
--we can consider this O(1) time.
--Given a pending tile set, takes in the index (fi, fj) of the first letter of the string
--and then moves to the right or down. If a blank tile is reache
--but the pending list has not been entirely hit, then we know tha
--the played word is not contiguous. Returns True if contiguous and False otherwise.
isPendContiguous : Int -> Int -> Direction -> List (Int, Int) -> Board -> Bool
isPendContiguous fi fj direction pendList b =
  case pendList of
    [] -> True
    p::ps ->
      if direction == Row then
        if (isCellEmpty fi fj b) then
          --we know that there's a discontinuity, but the pending list has not been consumed
          False
        else if fi == 14 then
          --we know that this must be the last tile and also pending (we are at the border)
          True
        else if p == (fi, fj) then
          --consume the head of the pending list since the cell we are at contains p
          isPendContiguous (fi+1) fj Row ps b
        else
          --don't consume the head since the cell we are at contains a permanent tile
          isPendContiguous (fi+1) fj Row pendList b
      else
        if (isCellEmpty fi fj b) then
          --we know that there's a discontinuity, but the pending list has not been consumed
          False
        else if fj == 14 then
          --we know that this must be the last tile and also pending (we are at the border)
          True
        else if p == (fi, fj) then
          --consume the head of the pending list since the cell we are at contains p
          isPendContiguous fi (fj+1) Col ps b
        else
          --don't consume the head since the cell we are at contains a permanent tile
          isPendContiguous fi (fj+1) Col pendList b

--Checks if any of the pending tiles are adjacent to a permanent cell. If yes, then return True. If not, return false.
isPendAdjacent : List (Int, Int) -> Matrix Cell -> Bool
isPendAdjacent pendList mat =
  case pendList of
    [] -> False
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
          _ -> False || (isPendAdjacent ps mat)

--Given a starting position (fi, fj), goes left or down until a blank tile/the board boundary
--is reached. Calculates the Just score if the word is valid, otherwise, return Nothing for the score.
--Output is the score (not multiplied by word multipliers yet), the String, and the word multiplier
--Note, letter multiplying is handled during traversal
calculateWord : Int -> Int -> Direction -> Board -> (Int, String, Int)
calculateWord fi fj direction (B pend mat) =
  if direction == Row then
    if isCellEmpty fi fj (B pend mat) || 15 <= fi then
      (0, "" , 1)
    else
      let
        cell = Matrix.get fi fj mat
        --The tuple from this call
        (currVal, currString, currMult) =
          (case cell of
            Just (C Normal (Pending (Tile.Letter char val))) -> (val, String.fromChar char, 1)
            --Use ! to denote blanks
            Just (C Normal (Pending Tile.Blank)) -> (0, "!", 1)
            Just (C Normal (Placed (Tile.Letter char val))) -> (val, String.fromChar char, 1)
            Just (C Normal (Placed Tile.Blank)) -> (0, "!", 1)
            Just (C (Premium (Letter mult)) (Pending (Tile.Letter char val))) -> ((val*mult), String.fromChar char, 1)
            Just (C (Premium (Letter mult))  (Pending Tile.Blank)) -> (0, "!", 1)
            Just (C (Premium (Letter mult)) (Placed (Tile.Letter char val))) -> ((val*mult), String.fromChar char, 1)
            Just (C (Premium (Letter mult))  (Placed Tile.Blank)) -> (0, "", 1)
            Just (C (Premium (Word mult)) (Pending (Tile.Letter char val))) -> (val, String.fromChar char, mult)
            Just (C (Premium (Word mult))  (Pending Tile.Blank)) -> (0, "!", mult)
            Just (C (Premium (Word mult)) (Placed (Tile.Letter char val))) -> (val, String.fromChar char, mult)
            Just (C (Premium (Word mult))  (Placed Tile.Blank)) -> (0, "!", mult)
            _ -> Debug.todo "calculateWord should not happen")
        --The tuple from recursive calls
        (recurVal, recurString, recurMult) = calculateWord (fi+1) fj Row (B pend mat)
      in
        (recurVal+currVal, currString ++ recurString , recurMult*currMult)
  else
    if isCellEmpty fi fj (B pend mat) || 15 <= fj then
      (0, "" , 1)
    else
      let
        cell = Matrix.get fi fj mat
        --The tuple from this call
        (currVal, currString, currMult) =
          (case cell of
            Just (C Normal (Pending (Tile.Letter char val))) -> (val, String.fromChar char, 1)
            --Use ! to denote blanks
            Just (C Normal (Pending Tile.Blank)) -> (0, "!", 1)
            Just (C Normal (Placed (Tile.Letter char val))) -> (val, String.fromChar char, 1)
            Just (C Normal (Placed Tile.Blank)) -> (0, "!", 1)
            Just (C (Premium (Letter mult)) (Pending (Tile.Letter char val))) -> ((val*mult), String.fromChar char, 1)
            Just (C (Premium (Letter mult))  (Pending Tile.Blank)) -> (0, "!", 1)
            Just (C (Premium (Letter mult)) (Placed (Tile.Letter char val))) -> ((val*mult), String.fromChar char, 1)
            Just (C (Premium (Letter mult))  (Placed Tile.Blank)) -> (0, "", 1)
            Just (C (Premium (Word mult)) (Pending (Tile.Letter char val))) -> (val, String.fromChar char, mult)
            Just (C (Premium (Word mult))  (Pending Tile.Blank)) -> (0, "!", mult)
            Just (C (Premium (Word mult)) (Placed (Tile.Letter char val))) -> (val, String.fromChar char, mult)
            Just (C (Premium (Word mult))  (Placed Tile.Blank)) -> (0, "!", mult)
            _ -> Debug.todo "calculateWord should not happen")
        --The tuple from recursive calls
        (recurVal, recurString, recurMult) = calculateWord fi (fj+1) Col (B pend mat)
      in
        (recurVal+currVal, currString ++ recurString , recurMult*currMult)

--Traverses the Pending word string (the first letter at (fi, fj)) and checks if the
--perpendicular words are valid. Returns Just total_score of the perpendicular words are all valid. Nothing otherwise.
traversePlayedWord : Int -> Int -> Direction -> Board -> Maybe Int
traversePlayedWord fi fj direction (B pend mat) =
  if direction == Row then
    let
      (perpi, perpj) = findFirstLetter fi fj Col (B pend mat)
    in
      case (traversePlayedWord fi (fj+1) Row (B pend mat), calculateWord perpi perpj Col (B pend mat)) of
        (Nothing, _) -> Nothing
        (Just val1, (val2, word, mult)) ->
          if String.length word <= 1 then
          --then no perpendicular word was formed and so only return val1
            Just val1
          else if RedBlackTree.member word dict then
            Just (val1 + val2*mult)
          else
            --The word formed was invalid
            Nothing
  else  --direction = Col
    let
      (perpi, perpj) = findFirstLetter fi fj Row (B pend mat)
    in
      case (traversePlayedWord fi (fj+1) Col (B pend mat), calculateWord perpi perpj Row (B pend mat)) of
        (Nothing, _) -> Nothing
        (Just val1, (val2, word, mult)) ->
          if String.length word <= 1 then
          --then no perpendicular word was formed and so only return val1
            Just val1
          else if RedBlackTree.member word dict then
            Just (val1 + val2*mult)
          else
            --The word formed was invalid
            Nothing
