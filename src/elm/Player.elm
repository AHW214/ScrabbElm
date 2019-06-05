module Player exposing (Player, init, decoder)

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