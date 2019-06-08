module Timer
  exposing
    ( Timer
    , new
    , tick
    , view
    )

import Time exposing (Posix)
import Html exposing (Html)
import Html.Attributes exposing (class)

type alias Timer =
  { value : Int
  , end : Int
  }

new : Int -> Posix -> Timer
new length now =
  Timer length (length + Time.posixToMillis now)

tick : Posix -> Timer -> Maybe Timer
tick now { end } =
  let
    nowMs = Time.posixToMillis now
    value = end - nowMs
  in
    if value > 0 then
      Just (Timer value end)
    else
      Nothing

view : Timer -> Html msg
view { value } =
  let
    posix = Time.millisToPosix value
    display f = String.padLeft 2 '0' << String.fromInt << f Time.utc
  in
    Html.div
      [ class "time" ]
      [ Html.text <| display Time.toMinute posix ++ ":" ++ display Time.toSecond posix ]