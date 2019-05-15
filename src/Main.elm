module Main exposing (..)

-- Imports

import Browser
import Browser.Events
import Html
import Html.Events
import Html.Attributes exposing (id, class)
import Http

import Events exposing (Msg(..))
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
  , held : Maybe Tile
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
  | ChoseTile Int
  | PendingTile Int Int

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

    ChoseTile index ->
      let (held, rack) = Rack.take index model.rack in
      ( { model
            | rack = rack
            , held = held
        }
      , Cmd.none
      )

    PendingTile i j ->
      ( case model.held of
          Nothing -> model
          Just tile ->
            { model
                | board = Board.setPending i j tile model.board
                , held = Nothing
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
            [ Board.view (ChoseTile 1) model.board
            , Rack.view model.rack
            ]
          ]
      ]
  }