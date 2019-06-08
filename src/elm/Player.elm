module Player
  exposing
    ( Player
    , init
    , setScore
    , decoder
    , view
    )

import Html exposing (Html)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)

type alias Player
  = { name : String
    , score : Int
    }

init : Player
init =
    Player "Anon" 0

decoder : Decoder Player
decoder =
  Decode.map2 Player
    (Decode.field "name" Decode.string)
    (Decode.field "score" Decode.int)

setScore : Int -> Player -> Player
setScore newScore player =
  { player | score = newScore }

view : Player -> Html msg
view { name, score } =
  Html.div
    [ class "player" ]
    [ Html.div
        [ class "name" ]
        [ Html.text name ]
    , Html.div
        [ class "score" ]
        [ Html.text <| String.fromInt score ]
    ]