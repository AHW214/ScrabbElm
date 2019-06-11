module Room
  exposing
    ( Room
    , join
    , leave
    )

import Player exposing (Player)

type alias Room =
  { name : String
  , players : List Player
  , currentPlayer : String
  }

join : Player -> Room -> Room
join pl room =
  { room | players = pl :: room.players }

leave : Player -> Room -> Room
leave { name } room =
  { room | players = List.filter ((/=) name << .name) room.players }