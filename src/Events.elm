module Events exposing (Msg(..))

import Http

type Msg
  = GotText (Result Http.Error String)
  | ChoseTile Int
  | PendingTile Int Int