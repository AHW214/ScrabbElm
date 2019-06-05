module Rack exposing
  ( Rack, size
  , empty, init
  , take, chooseToExchange
  , return, replenish
  , exchange, exchanging
  , tilesToExchange, allEmpty
  , updateBlank, view
  , allChosenBlank
  )

import Array exposing (Array)
import Html exposing (Html)
import Html.Events exposing (onClick, preventDefaultOn)
import Html.Attributes exposing (id, class)
import Json.Decode as Decode

import Tile exposing (Tile)

type alias Event msg
  = Int -> msg

type alias Events msg
  = { placeEv : Event msg
    , exchangeEv : Event msg
    }

type alias Chosen
  = Bool

type Cell
  = Empty
  | Occupied Chosen Tile

type Mode
  = Place
  | Exchange

type Rack
  = R Mode (Array Cell)

size : Int
size = 7

exchanging : Rack -> Bool
exchanging (R mode _) =
  case mode of
    Exchange ->
      True
    Place ->
      False

getChosenTile : Cell -> Maybe Tile
getChosenTile cell =
  case cell of
    Occupied True tile ->
      Just tile
    _ ->
      Nothing

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

allChosenBlank : Rack -> Bool
allChosenBlank (R _ cells) =
  let
    blanks =
      Array.filter (\c ->
        case getChosenTile c of
          Just tile ->
            Tile.isBlank tile
          _ ->
            False
      ) cells
  in
    Array.length blanks == Tile.numBlanks

isEmpty : Cell -> Bool
isEmpty cell =
  case cell of
    Empty ->
      True
    _ ->
      False

allEmpty : Rack -> Bool
allEmpty (R _ cells) =
  let
    allArray f =
      Array.foldl ((&&) << f) True
  in
    allArray isEmpty cells

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
    Just (Occupied False tile) ->
      (Just (index, tile), R mode (Array.set index (Occupied True tile) cells))
    _ ->
      (Nothing, R mode cells)

return : Int -> Rack -> Rack
return index (R mode cells) =
  case Array.get index cells of
    Just (Occupied True tile) ->
      let
        newTile =
          if Tile.isBlank tile then
            Tile.blank
          else
            tile
      in
        R mode (Array.set index (Occupied False newTile) cells)
    _ ->
      R mode cells

updateBlank : Int -> Maybe Char -> Rack -> Rack
updateBlank index mc (R mode cells) =
  case Array.get index cells of
    Just (Occupied True tile) ->
      if Tile.isBlank tile then
        let
          updatedTile = Tile.blankFrom mc
        in
          R mode (Array.set index (Occupied True updatedTile) cells)
      else
        R mode cells
    _ ->
      R mode cells

chooseToExchange : Int -> Rack -> Rack
chooseToExchange index (R mode cells) =
  case Array.get index cells of
    Just (Occupied chosen tile) ->
      let
        newCells =
          Array.set index (Occupied (not chosen) tile) cells
        newMode =
          if anyChosen newCells then
            Exchange
          else
            Place
      in
        R newMode newCells
    _ ->
      R mode cells

replenish : List Tile -> Rack -> (Rack, List Tile)
replenish bag (R _ cells) =
  Array.foldl
    (\cell (newCells, newBag) ->
      case cell of
        Occupied True _ ->
          case List.head newBag of
            Nothing ->
              (Array.push Empty newCells, newBag)
            Just newTile ->
              (Array.push (Occupied False newTile) newCells, List.drop 1 newBag)
        _ ->
          (Array.push cell newCells, newBag)
    )
    (Array.empty, bag) cells
  |> Tuple.mapFirst (R Place)

exchange : List Tile -> Rack -> Rack
exchange tiles (R _ cells) =
  Array.foldl
    (\cell (newCells, newTiles) ->
      case (newTiles, cell) of
        (tile::restTiles, Occupied True _) ->
          (Array.push (Occupied False tile) newCells, restTiles)
        _ ->
          (Array.push cell newCells, newTiles)
    )
    (Array.empty, tiles) cells
  |> Tuple.first
  |> R Place

tilesToExchange : Rack -> List Tile
tilesToExchange (R mode cells) =
  case mode of
    Place ->
      []
    Exchange ->
      Array.toList cells
        |> List.filterMap getChosenTile


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

view : Maybe (Events msg) -> Rack -> Html msg
view maybeEvs (R mode cells) =
  let
    handlers i =
      case maybeEvs of
        Nothing ->
          []
        Just { placeEv, exchangeEv } ->
          case mode of
            Place ->
              if anyChosen cells then
                [ onClick (placeEv i) ]
              else
                [ onClick (placeEv i)
                , onRightClick (exchangeEv i)
                ]
            Exchange ->
              [ onRightClick (exchangeEv i) ]

    modeStr =
      case mode of
        Place ->
          "place"
        Exchange ->
          "exchange"
  in
    Html.div
    [ id "rack", class "no-select", class modeStr ]
    (cells
      |> Array.indexedMap (viewCell << handlers)
      |> Array.toList)

onRightClick : msg -> Html.Attribute msg
onRightClick event =
  preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed event))

alwaysPreventDefault : msg -> (msg, Bool)
alwaysPreventDefault msg =
  (msg, True)