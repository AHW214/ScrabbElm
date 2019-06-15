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

updatePlayer : Player -> Room -> Room
updatePlayer player room =
  let
    update =
      List.foldl (\p ps ->
        if p.name == player.name then
          player :: ps
        else
          p :: ps
      ) []
  in
    { room | players = update room.players }

