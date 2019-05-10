module RedBlackTree exposing (..)

type Color  = R | B
type Tree a = E | T Color (Tree a) a (Tree a)

type alias Balance comparable =
  Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable

empty : Tree a
empty = E

member : comparable -> Tree comparable -> Bool
member x t =
  case t of
    E -> False
    T _ l y r ->
      if x == y then
        True
      else if x < y then
        member x l
      else
        member x r

insert : comparable -> Tree comparable -> Tree comparable
insert x t =
  case ins x t of
    (T _ l y r, _) -> T B l y r
    (E, _)         -> Debug.todo "insert: Impossible"

fromList : List comparable -> Tree comparable
fromList = List.foldl insert empty

-----------------------------------------------------------------

ins : comparable
   -> Tree comparable
   -> (Tree comparable, (Balance comparable, Balance comparable))
ins x t =
  case t of
    E -> (T R E x E, (T, T))
    T color l y r ->
      if x == y then
        (t, (T, T))
      else if x < y then
        let (l1, (b, _)) = ins x l in
        (b color l1 y r, (balanceLL, balanceRL))
      else
        let (r1, (_, b)) = ins x r in
        (b color l y r1, (balanceLR, balanceRR))


balanceLL : Balance comparable
balanceLL color l val r =
  case (color, (l, val, r)) of
    (B, (T R (T R a x b) y c, z, d)) -> T R (T B a x b) y (T B c z d)
    _                                -> T color l val r

balanceLR : Balance comparable
balanceLR color l val r =
  case (color, (l, val, r)) of
    (B, (T R a x (T R b y c), z, d)) -> T R (T B a x b) y (T B c z d)
    _                                -> T color l val r

balanceRL : Balance comparable
balanceRL color l val r =
  case (color, (l, val, r)) of
    (B, (a, x, T R (T R b y c) z d)) -> T R (T B a x b) y (T B c z d)
    _                                -> T color l val r

balanceRR : Balance comparable
balanceRR color l val r =
  case (color, (l, val, r)) of
    (B, (a, x, T R b y (T R c z d))) -> T R (T B a x b) y (T B c z d)
    _                                -> T color l val r