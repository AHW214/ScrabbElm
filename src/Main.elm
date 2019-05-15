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
  , held : Maybe (Int, Tile)
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( { dict = empty
            , board = Board.init
            , rack = Rack.init
            , held = Nothing
            }
          , Http.get
            { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
            , expect = Http.expectString GotText
            }
          )


-- Update

type Msg
  = GotText (Result Http.Error String)
  | TookTile Int
  | SetPendingTile Int Int
  | RemPendingTile Int Int
  | EndTurn

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fulltext ->
          ( { model | dict = loadDictionary fulltext }
          , Cmd.none
          )
        Err _ ->
          ( model
          , Cmd.none
          )

    TookTile index ->
      ( case model.held of
        Nothing ->
          let (taken, newRack) = Rack.take index model.rack in
          { model
            | held = taken
            , rack = newRack
          }
        _ -> model
      , Cmd.none
      )

    SetPendingTile i j ->
      ( case model.held of
          Nothing -> model
          Just rackTile ->
            let (board, _) = Board.setPending i j rackTile model.board in
            { model
                | board = board
                , held = Nothing
            }
      , Cmd.none
      )

    RemPendingTile i j ->
      let
        (newBoard, newRack) =
          case model.held of
            Nothing ->
              let (board, mi) = Board.removePending i j model.board in
              (board
              , case mi of
                  Nothing ->
                    model.rack
                  Just index ->
                    Rack.return index model.rack
              )
            Just rackTile ->
              let
                (_, mi) = Board.removePending i j model.board
                (board, _) = Board.setPending i j rackTile model.board
              in
                (board
                , case mi of
                    Nothing ->
                      model.rack
                    Just index ->
                      Rack.return index model.rack
                )
      in
      ( { model
          | board = newBoard
          , rack = newRack
          , held = Nothing
        }
      , Cmd.none
      )

    EndTurn ->
      ( { model
          | board = Board.placePending model.board
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
  { title = "ScrabbElm"
  , body =
      [ Html.div
          [ id "wrapper" ]
          [ Html.div
            [ class "centered" ]
            [ Board.view
                { evEmpty = SetPendingTile
                , evPending = RemPendingTile
                }
                model.board
            , Rack.view TookTile model.rack
            ]
          ]
      ]
  }