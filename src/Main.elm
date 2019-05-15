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
  | ClickedRack Int
  | ClickedBoard Int Int
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

    ClickedRack i ->
      ( let
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
          { model
            | held = taken
            , rack = newRack
          }
      , Cmd.none
      )

    ClickedBoard i j ->
      let
        (newBoard, maybeRet) = Board.set i j model.held model.board
      in
        ( { model
            | board = newBoard
            , held = Nothing
            , rack =
                case maybeRet of
                  Nothing ->
                    model.rack
                  Just index ->
                    Rack.return index model.rack
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
            [ Board.view ClickedBoard model.board
            , Rack.view ClickedRack model.rack
            ]
          ]
      ]
  }