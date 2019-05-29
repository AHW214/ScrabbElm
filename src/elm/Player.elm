module Player exposing (..)

import Rack as R

type alias Player = { rack : R.Rack, points : Int }

newPlayer : Player 
newPlayer = { rack = R.empty, points = 0 }




