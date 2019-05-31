module Main exposing (..)

-- Imports

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, class)
import Http
import Random exposing (Generator)
import Random.List

import RedBlackTree exposing (Tree, empty, insert, member)
import WebSocket exposing (ConnectionInfo)
import Board exposing (Board)
import Rack exposing (Rack)
import Tile exposing (Tile)


-- Program

type alias Flags = ()

main : Program Flags Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type SocketStatus
  = Unopened
  | Connected ConnectionInfo
  | Closed Int

type alias Model =
  { socketInfo: SocketStatus
  , dict : Tree String
  , board : Board
  , rack : Rack
  , bag : List Tile
  , held : Maybe (Int, Tile)
  , turnScore : Maybe Int
  , totalScore : Int
  , boardEmpty : Bool
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( { socketInfo = Unopened
            , dict = empty
            , board = Board.init
            , rack = Rack.empty
            , bag = List.map Tile.letter (String.toList "historyhornfarmpastemobbitamsndlnfdsl")
            , held = Nothing
            --Changed from Nothing as that more accurately depicts a move without doing anything
            , turnScore = Just 0
            , totalScore = 0
            , boardEmpty = True
            }
          , Cmd.batch
              [ WebSocket.connect "wss://echo.websocket.org" []
              , Http.get
                  { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
                  , expect = Http.expectString GotText
                  }
              ]
          )


-- Update

type Msg
  = SocketConnect ConnectionInfo
  | SocketClosed Int
  | ReceivedString String
  | Error String
  | GotText (Result Http.Error String)
  | GotBag (List Tile)
  | ChoseRackPlace Int
  | ChoseRackExchange Int
  | ClickedBoard Int Int
  | EndExchange (Maybe (List Tile, List Tile))
  | StartExchange
  | PassTurn
  | EndTurn

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SocketConnect info ->
      ( { model | socketInfo = Connected info }
      , Cmd.none
      )

    SocketClosed code ->
      ( { model | socketInfo = Closed code }
      , Cmd.none
      )

    ReceivedString message ->
      Debug.todo "TODO"

    Error errMsg ->
      let
        _ = Debug.log "Error" errMsg
      in
      ( model
      , Cmd.none
      )

    GotText result ->
      let
        newModel =
          case result of
            Ok fulltext ->
              { model | dict = loadDictionary fulltext }
            Err _ ->
              model
      in
        ( newModel
        , Cmd.none
        )

    GotBag bag ->
      let
        (newRack, newBag) = Rack.init model.bag
      in
        ( { model
            | rack = newRack
            , bag = newBag
          }
        , Cmd.none
        )

    ChoseRackPlace i ->
      let
        (taken, newRack) =
          case model.held of
            Nothing ->
              Rack.take i model.rack
            Just (j, _) ->
              if j == i then
                ( Nothing
                , Rack.return i model.rack
                )
              else
                model.rack
                |> Rack.return j
                |> Rack.take i
      in
        ( { model
            | held = taken
            , rack = newRack
          }
        , Cmd.none
        )

    ChoseRackExchange i ->
      let
        newRack =
          Rack.chooseToExchange i model.rack
      in
        ( { model | rack = newRack }
        , Cmd.none
        )

    ClickedBoard i j ->
      let
        (newBoard, maybeRet) =
          Board.set i j model.held model.board

        validator =
          if model.boardEmpty then
            Board.pendingTilesCheckFirstTurn
          else
            Board.pendingTilesWordCheck

        newTurnScore =
          Debug.log "SCORE" <| validator model.dict newBoard

        newRack =
          case maybeRet of
            Nothing ->
              model.rack
            Just index ->
              Rack.return index model.rack
      in
        ( { model
            | board = newBoard
            , held = Nothing
            , rack = newRack
            , turnScore = newTurnScore
          }
        , Cmd.none
        )

    EndTurn ->
      let
        (newRack, newBag) =
          Rack.replenish model.bag model.rack

        (newBoard, placed) =
          Board.placePending model.board

        turnScore =
          case model.turnScore of
            Nothing -> 0
            Just score ->
              if placed == Rack.size then
                score + 50
              else
                score

        newScore =
          model.totalScore + turnScore

        boardE =
          if model.boardEmpty == False then
            False
          else
            case Board.getTileAt 7 7 newBoard of
              Just _ ->
                False
              _ ->
                model.boardEmpty
      in
        ( { model
            | board = newBoard
            , rack = newRack
            , bag = newBag
            , turnScore = Just 0
            , totalScore = Debug.log "TOTAL SCORE" newScore
            , boardEmpty = boardE
          }
        , Cmd.none
        )

    StartExchange ->
      let
        tiles =
          Rack.tilesToExchange model.rack
      in
        ( model
        , Random.generate
            EndExchange (exchangeTiles tiles model.bag)
        )

    EndExchange result ->
      let
        newModel =
          case result of
            Nothing ->
              model
            Just (exTiles, exBag) ->
              { model
                  | rack = Rack.exchange exTiles model.rack
                  , bag = exBag
              }
      in
        ( newModel
        , Cmd.none
        )

    PassTurn ->
      ( model
      , Cmd.none
      )


loadDictionary : String -> Tree String
loadDictionary = RedBlackTree.fromList << String.words

exchangeTiles : List Tile -> List Tile -> Generator (Maybe (List Tile, List Tile))
exchangeTiles discarded bag =
  chooseRandomTiles (List.length discarded) ([], bag)
    |> Random.andThen
      (\result ->
        case result of
          Nothing ->
            Random.constant Nothing
          Just (chosen, newBag) ->
            Random.map2
              (\x y -> Just (x, y))
              (Random.constant chosen)
              (Random.List.shuffle (discarded ++ newBag))
      )

chooseRandomTiles : Int -> (List Tile, List Tile) -> Generator (Maybe (List Tile, List Tile))
chooseRandomTiles i (chosen, bag) =
  if i <= 0 then
    Random.constant (Just (chosen, bag))
  else
    Random.List.choose bag
      |> Random.andThen
        (\(maybeTile, newBag) ->
          case maybeTile of
            Nothing ->
              Random.constant Nothing
            Just tile ->
              Random.lazy (\_ -> chooseRandomTiles (i - 1) (tile :: chosen, newBag))
        )


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.events
    (\event ->
        case event of
          WebSocket.Connected info ->
            SocketConnect info

          WebSocket.StringMessage info message ->
            ReceivedString message

          WebSocket.Closed _ unsentBytes ->
            SocketClosed unsentBytes

          WebSocket.Error _ code ->
            Error ("WebSocket Error: " ++ String.fromInt code)

          WebSocket.BadMessage error ->
            Error error
    )


-- View

viewTurn : Model -> Html Msg
viewTurn { rack, bag, turnScore } =
  let
    (attr, html) =
      if Rack.exchanging rack then
        if List.length bag < Rack.size then
          ([], [ Html.text "Not enough tiles..." ])
        else
          ([ onClick StartExchange ], [ Html.text "Exchange Tiles." ])
      else
        case turnScore of
          Nothing ->
            ([], [ Html.text "Finish your move..." ])
          Just score ->
            if score > 0 then
              ([ onClick EndTurn ], [ Html.text "End turn." ])
            else
              ([ onClick PassTurn ], [ Html.text "Pass turn." ])
  in
    Html.button attr html

viewScore : Int -> Html Msg
viewScore s =
  Html.div [ id "score" ] [ Html.text (String.fromInt s) ]

view : Model -> Browser.Document Msg
view model =
  { title = "ScrabbElm"
  , body =
      [ Html.div
          [ id "wrapper" ]
          [ Html.div
            [ class "centered" ]
            [ Board.view ClickedBoard model.held model.board
            , Rack.view { placeEv = ChoseRackPlace, exchangeEv = ChoseRackExchange } model.rack
            , Html.button [ Html.Events.onClick (GotBag model.bag) ] [ Html.text "init rack" ]
            , viewTurn model
            ]
          , viewScore model.totalScore
          ]
      ]
  }