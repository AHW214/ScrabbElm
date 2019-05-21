module Main exposing (..)

-- Imports

import Browser
import Browser.Events
import Html
import Html.Events
import Html.Attributes exposing (id, class)
import Http

import RedBlackTree exposing (Tree, empty, insert, member)
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

type alias Model =
  { dict : Tree String
  , board : Board
  , rack : Rack
  , bag : List Tile
  , held : Maybe (Int, Tile)
  , turnScore : Maybe Int
  , boardEmpty : Bool
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( { dict = empty
            , board = Board.init
            , rack = Rack.empty
            , bag = List.map Tile.letter (String.toList "pasteheedoodefghijklmnopqrstuvwxyz")
            , held = Nothing
            --Changed from Nothing as that more accurately depicts a move without doing anything
            , turnScore = Just 0
            , boardEmpty = True
            }
          , Http.get
            { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
            , expect = Http.expectString GotText
            }
          )


-- Update

type Msg
  = GotText (Result Http.Error String)
  | GotBag (List Tile)
  | ClickedRack Int
  | ClickedBoard Int Int
  | EndTurn

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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

    ClickedRack i ->
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

    ClickedBoard i j ->
      let
        (newBoard, maybeRet) = Board.set i j model.held model.board
        newScore = 
          Debug.log "SCORE" <| (if model.boardEmpty then         
            Board.pendingTilesCheckFirstTurn model.dict newBoard
          else
            Board.pendingTilesWordCheck model.dict newBoard)
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
            , turnScore = newScore
          }
        , Cmd.none
        )

    EndTurn ->
      let
        (newRack, newBag) = Rack.replenish model.bag model.rack
        newBoard = Board.placePending model.board
        boardE = 
          (if model.boardEmpty == False then
            False
          else 
            case Board.getTileAt 7 7 newBoard of
              Just _ -> False
              _ -> model.boardEmpty)
      in
        ( { model
            | board = newBoard
            , rack = newRack
            , bag = newBag
            , boardEmpty = boardE
          }
        , Cmd.none
        )


loadDictionary : String -> Tree String
loadDictionary = RedBlackTree.fromList << String.words

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- View

view : Model -> Browser.Document Msg
view model =
  let
    (attr, html) =
      case model.turnScore of
        Nothing ->
          ([], [ Html.text "move invalid"])
        Just _ ->
          ([ Html.Events.onClick EndTurn ], [ Html.text "end turn" ])
  in
    { title = "ScrabbElm"
    , body =
        [ Html.div
            [ id "wrapper" ]
            [ Html.div
              [ class "centered" ]
              [ Board.view ClickedBoard model.held model.board
              , Rack.view ClickedRack model.rack
              , Html.button [ Html.Events.onClick (GotBag model.bag) ] [ Html.text "init rack" ]
              , Html.button attr html
              ]
            ]
        ]
    }