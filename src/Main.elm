module Main exposing (..)

-- Imports

import Browser
import Browser.Events
import Html
import Html.Events
import Http

import RedBlackTree exposing (Tree, empty, insert, member)


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
  , inDict : Bool
  }

init : Flags -> ( Model, Cmd Msg )
init () = ( { dict = empty
            , inDict = True
            }
          , Http.get
            { url = "https://raw.githubusercontent.com/AHW214/ScrabbElm/master/assets/dictionary.txt"
            , expect = Http.expectString GotText
            }
          )


-- Update

type Msg
  = GotText (Result Http.Error String)
  | CheckWord String

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

    CheckWord word ->
      ( { model | inDict = member word model.dict }
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
    [ Html.input [ Html.Events.onInput CheckWord ] []
    , Html.h1 [] [Html.text <| Debug.toString model.inDict ]
    ]
  }