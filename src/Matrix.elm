module Matrix exposing (..)

import Array exposing (Array)


type Matrix a = M (Array (Array a))

repeat : Int -> Int -> a -> Matrix a
repeat w h x = M (Array.repeat h (Array.repeat w x))

get : Int -> Int -> Matrix a -> Maybe a
get i j (M rows) =
  Array.get i rows |> Maybe.andThen (Array.get j)

set : Int -> Int -> a -> Matrix a -> Matrix a
set i j x (M rows) =
  case Array.get i rows of
    Nothing  -> M rows
    Just row ->
      M (Array.set i (Array.set j x row) rows)

map : (a -> b) -> Matrix a -> Matrix b
map fun (M rows) =
  M (Array.map (Array.map fun) rows)

indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap fun (M rows) =
  M (Array.indexedMap (\i -> Array.indexedMap (fun i)) rows)

toLists : Matrix a -> List (List a)
toLists (M rows) =
  Array.toList (Array.map Array.toList rows)