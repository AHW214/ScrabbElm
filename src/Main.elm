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
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( { dict = empty
            , board = Board.init
            , rack = Rack.init
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
      ( { model | rack = Debug.log "a" <| Rack.take index model.rack }
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
            , Rack.view ChoseTile model.rack
            ]
          ]
      ]
  }