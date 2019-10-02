module ListWrapper.Set exposing
  ( Set
  , empty, singleton, insert, remove
  , isEmpty, member, size, eq
  , union, intersect, diff
  , toList, fromList
  , map, fold, filter, partition
  )

{-| A set implementation using `List`. The elements can be any type includes
recodes and custom type. This module provides all functions does `Set`, except
'foldl', 'foldr', 'merge'.

Insert, remove, and query operations all take *O(n)* time. Only for a few
elements.

To determine equality, use 'eq' instead of '(==)'.

# Sets
@docs Set

# Build
@docs empty, singleton, insert, remove

# Query
@docs isEmpty, member, size, eq

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, fold, filter, partition

-}


{-| A set of unique values. -}
type Set e =
  Set (List e)

{-| Create an empty set. -}
empty : Set e
empty =
  Set []


{-| Create a set with one value. -}
singleton : e -> Set e
singleton element =
  Set [ ( element ) ]


{-| Insert a value into a set. -}
insert : e -> Set e -> Set e
insert element set =
  if member element set
  then
    set
  else
    let (Set list) = set in
    Set (element :: list)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : e -> Set e -> Set e
remove element (Set list) =
  Set (removeHelp element list [])

removeHelp : e -> List e -> List e -> List e
removeHelp element rest result =
  case rest of
    [] ->
      result

    hd :: tl ->
      if hd == element
      then result ++ tl
      else removeHelp element tl (hd :: result)


{-| Determine if a set is empty.
-}
isEmpty : Set e -> Bool
isEmpty (Set list) =
  List.isEmpty list


{-| Determine if a value is in a set. -}
member : e -> Set e -> Bool
member element (Set list) =
  List.member element list


{-| Determine the number of elements in a set.
-}
size : Set e -> Int
size (Set list) =
  List.length list


{-| Determine if given two dictionary are the same. -}
eq : Set e -> Set e -> Bool
eq (Set l1) (Set l2) =
  if List.length l1 /= List.length l2
  then False
  else eqHelp l1 l2

eqHelp : List e -> List e -> Bool
eqHelp l1 l2 =
  case l1 of
    [] ->
      True

    hd :: tl ->
      if List.member hd l2
      then eqHelp tl l2
      else False


{-| Get the union of two sets. Keep all values.
-}
union : Set e -> Set e -> Set e
union s1 s2 =
  fold insert s2 s1


{-| Fold over the key-value pairs in a dictionary. The order is not fixed. -}
fold : (e -> a -> a) -> a -> Set e -> a
fold func acc (Set list) =
  List.foldl func acc list


{-| Get the intersection of two sets. Keeps values that appear in both sets. -}
intersect : Set e -> Set e -> Set e
intersect s1 s2 =
  filter (\e -> member e s2) s1


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set e -> Set e -> Set e
diff s1 s2 =
  fold remove s1 s2


{-| Convert a set into a list. The order is not Fixed. -}
toList : Set a -> List a
toList (Set list) =
  list


{-| Convert a list into a set, removing any duplicates. -}
fromList : List e -> Set e
fromList assocs =
  List.foldl insert empty assocs


{-| Map a function onto a set, creating a new set with no duplicates. -}
map : (e1 -> e2) -> Set e1 -> Set e2
map func set =
  fold (\e -> insert (func e)) empty set


{-| Only keep elements that pass the given test. -}
filter : (e -> Bool) -> Set e -> Set e
filter isGood set =
  Set (fold (\e ls -> if isGood e then e :: ls else ls) [] set)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not. -}
partition : (e -> Bool) -> Set e -> (Set e, Set e)
partition isGood set =
  let
    func e ( l1, l2 ) =
      if isGood e
      then ( e :: l1, l2 )
      else ( l1, e :: l2 )

    ( s1, s2 ) =
      fold func ( [], [] ) set
  in
    ( Set s1, Set s2 )