module ListWrapper.Dict exposing
  ( Dict
  , empty, singleton, insert, update, remove
  , isEmpty, member, get, size, eq
  , keys, values, toList, fromList
  , map, fold, filter, partition
  , union, intersect, diff
  )

{-| A dictionary implementation using `List`. The keys can be any type includes
recodes and custom type. This module provides all functions does `Dict`, except
'foldl', 'foldr', 'merge'.

Insert, remove, and query operations all take *O(n)* time. Only for a few
elements.

To determine equality, use 'eq' instead of '(==)'.

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size, eq

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, fold, filter, partition

# Combine
@docs union, intersect, diff

-}


type alias Wraped k v = List ( k, v )


{-| A dictionary of keys and values. -}
type Dict k v =
  Dict (Wraped k v)


{-| Create an empty dictionary. -}

empty : Dict k v
empty =
  Dict []


{-| Create a dictionary with one key-value pair. -}
singleton : k -> v -> Dict k v
singleton key value =
  Dict [ ( key, value ) ]


{-| Get the value associated with a key. -}
get : k -> Dict k v -> Maybe v
get key (Dict list) =
  getHelp key list

getHelp : k -> Wraped k v -> Maybe v
getHelp key list =
  case list of
    [] ->
      Nothing

    ( k, v ) :: tl ->
      if k == key
      then Just v
      else getHelp key tl


{-| Determine if a key is in a dictionary. -}
member : k -> Dict k v -> Bool
member key dict =
  case get key dict of
    Just _ ->
      True

    Nothing ->
      False


{-| Determine the number of key-value pairs in the dictionary. -}
size : Dict k v -> Int
size (Dict list) =
  List.length list


{-| Determine if a dictionary is empty. -}
isEmpty : Dict k v -> Bool
isEmpty (Dict list) =
  case list of
    [] ->
      True

    _ ->
      False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : k -> v -> Dict k v -> Dict k v
insert key value (Dict list) =
  Dict (insertHelp key value list [])

insertHelp : k -> v -> Wraped k v -> Wraped k v -> Wraped k v
insertHelp key value rest result =
  case rest of
    [] ->
      ( key, value ) :: result

    hd :: tl ->
      let ( k, v ) = hd in
      if k == key
      then ( key, value ) :: result ++ tl
      else insertHelp key value tl (hd :: result)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : k -> Dict k v -> Dict k v
remove key (Dict list) =
  Dict (removeHelp key list [])

removeHelp : k -> Wraped k v -> Wraped k v -> Wraped k v
removeHelp key rest result =
  case rest of
    [] ->
      result

    ( k, v ) :: tl ->
      if k == key
      then result ++ tl
      else removeHelp key tl (( k, v ) :: result)


{-| Update the value of a dictionary for a specific key
with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key alter (Dict list) =
  Dict (updateHelp key alter list [])

updateHelp : k -> (Maybe v -> Maybe v) -> Wraped k v -> Wraped k v -> Wraped k v
updateHelp key alter rest result =
  case rest of
    [] ->
      result

    ( k, v ) :: tl ->
      if k == key
      then case alter (Just v) of
        Nothing ->
          result ++ tl

        Just u ->
          ( key, u ) :: result ++ tl
      else updateHelp key alter tl (( k, v ) :: result)


{-| Apply a function to all values in a dictionary. -}
map : (k -> a -> b) -> Dict k a -> Dict k b
map func dict =
  Dict (fold (\k v ls -> ( k, func k v ) :: ls) [] dict)


{-| Fold over the key-value pairs in a dictionary. The order is not fixed. -}
fold : (k -> v -> b -> b) -> b -> Dict k v -> b
fold func acc (Dict list) =
  List.foldl (\( k, v ) -> func k v) acc list


{-| Keep only the key-value pairs that pass the given test. -}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter isGood dict =
  Dict (fold (\k v ls -> if isGood k v then ( k, v ) :: ls else ls) [] dict)


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}

partition : (k -> v -> Bool) -> Dict k v -> (Dict k v, Dict k v)
partition isGood dict =
  let
    func k v ( l1, l2 ) =
      if isGood k v
      then ( ( k, v ) :: l1, l2 )
      else ( l1, ( k, v ) :: l2 )

    ( d1, d2 ) =
      fold func ( [], [] ) dict
  in
    ( Dict d1, Dict d2 )


{-| Determine if given two dictionary are the same. -}
eq : Dict k v -> Dict k v -> Bool
eq (Dict l1) (Dict l2) =
  if List.length l1 /= List.length l2
  then False
  else eqHelp l1 (Dict l2)

eqHelp : Wraped k v -> Dict k v -> Bool
eqHelp list dict =
  case list of
    [] ->
      True

    ( k, _ ) :: tl ->
      if member k dict
      then eqHelp tl dict
      else False


-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union d1 d2 =
  fold insert d2 d1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect d1 d2 =
  filter (\k _ -> member k d2) d1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict k a -> Dict k b -> Dict k a
diff d1 d2 =
  fold (\k _ -> remove k) d1 d2


-- LIST


{-| Get all of the keys in a dictionary. The order is not Fixed. -}
keys : Dict k v -> List k
keys dict =
  fold (\k _ ls -> k :: ls) [] dict


{-| Get all of the values in a dictionary. The order is not Fixed. -}
values : Dict k v -> List v
values dict =
  fold (\_ v ls -> v :: ls) [] dict


{-| Convert a dictionary into an association list of key-value pairs.
The order is not Fixed.
-}

toList : Dict k v -> List ( k, v )
toList (Dict list) =
  list


{-| Convert an association list into a dictionary. This function takes *O(n^2)*
for n-element list.
-}
fromList : List ( k, v ) -> Dict k v
fromList assocs =
  List.foldl (\( k, v ) -> insert k v) empty assocs
