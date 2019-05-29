module Rack exposing
  ( Rack, size
  , empty, init
  , take, chooseToDiscard
  , return, replenish
  , discarding, view
  )

import Array exposing (Array)
import Html exposing (Html)
import Html.Events exposing (onClick, preventDefaultOn)
import Html.Attributes exposing (class)
import Json.Decode as Decode

import Tile exposing (Tile)

type alias Event msg
  = Int -> msg

type alias Events msg
  = { placeEv : Event msg
    , discardEv : Event msg
    }

type alias Chosen
  = Bool

type Cell
  = Empty
  | Occupied Chosen Tile

type Mode
  = Place
  | Discard

type Rack
  = R Mode (Array Cell)

size : Int
size = 7

discarding : Rack -> Bool
discarding (R mode _) =
  case mode of
    Discard -> True
    Place   -> False

isChosen : Cell -> Bool
isChosen cell =
  case cell of
    Empty ->
      False
    Occupied chosen _ ->
      chosen

anyChosen : Array Cell -> Bool
anyChosen cells =
  let
    anyArray f =
      Array.foldl ((||) << f) False
  in
    anyArray isChosen cells

empty : Rack
empty = R Place (Array.repeat size Empty)

-- assumes bag has over seven tiles
init : List Tile -> (Rack, List Tile)
init bag =
  ( bag
    |> List.take size
    |> List.map (Occupied False)
    |> Array.fromList
    |> R Place
  , List.drop size bag
  )

take : Int -> Rack -> (Maybe (Int, Tile), Rack)
take index (R mode cells) =
  case Array.get index cells of
    Nothing    -> (Nothing, R mode cells)
    Just Empty -> (Nothing, R mode cells)
    Just (Occupied chosen tile) ->
      case chosen of
        True ->
          (Nothing, R mode cells)
        False ->
          (Just (index, tile), R mode (Array.set index (Occupied True tile) cells))

return : Int -> Rack -> Rack
return index (R mode cells) =
  case Array.get index cells of
    Nothing    -> R mode cells
    Just Empty -> R mode cells
    Just (Occupied chosen tile) ->
      case chosen of
        True ->
          R mode (Array.set index (Occupied False tile) cells)
        False ->
          R mode cells

chooseToDiscard : Int -> Rack -> Rack
chooseToDiscard index (R mode cells) =
  case Array.get index cells of
    Nothing    ->
      R mode cells
    Just Empty ->
      R mode cells
    Just (Occupied chosen tile) ->
      let
        newCells =
          Array.set index (Occupied (not chosen) tile) cells
        newMode =
          if anyChosen newCells then
            Discard
          else
            Place
      in
        R newMode newCells

replenish : List Tile -> Rack -> (Rack, List Tile)
replenish bag (R _ cells) =
  Array.foldl
    (\cell (newCells, newBag) ->
      case cell of
        Empty ->
          (Array.push cell newCells, newBag)
        Occupied False tile ->
          (Array.push cell newCells, newBag)
        Occupied True tile ->
          case List.head newBag of
            Nothing ->
              (Array.push Empty newCells, newBag)
            Just newTile ->
              (Array.push (Occupied False newTile) newCells, List.drop 1 newBag)
    )
    (Array.empty, bag) cells
  |> Tuple.mapFirst (R Place)

viewCell : List (Html.Attribute msg) -> Cell -> Html msg
viewCell handlers cell =
  let
    (attr, html) =
      case cell of
        Empty ->
          ( [ class "empty" ]
          , []
          )
        Occupied chosen tile ->
          ( if chosen then
              [ class "chosen" ]
            else
              []
          , [ Tile.view tile ]
          )
  in
    Html.div
      (attr ++ handlers)
      html

view : Events msg -> Rack -> Html msg
view { placeEv, discardEv } (R mode cells) =
  let
    handlers i =
      case mode of
        Place ->
          if anyChosen cells then
            [ onClick (placeEv i) ]
          else
            [ onClick (placeEv i)
            , onRightClick (discardEv i)
            ]
        Discard ->
          [ onRightClick (discardEv i) ]

    modeStr =
      case mode of
        Place ->
          "place"
        Discard ->
          "discard"
  in
    Html.div
    [ class "rack", class modeStr ]
    (cells
      |> Array.indexedMap (viewCell << handlers)
      |> Array.toList)

onRightClick : msg -> Html.Attribute msg
onRightClick event =
  preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed event))

alwaysPreventDefault : msg -> (msg, Bool)
alwaysPreventDefault msg =
  (msg, True)