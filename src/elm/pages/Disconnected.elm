module Page.Disconnected
  exposing
    ( view )


-- imports

import Html exposing (Html)
import Html.Attributes as HA


-- view

view : Html msg
view =
  Html.div [] [ Html.text "disconnected" ]