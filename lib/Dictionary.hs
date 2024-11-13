{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Dictionary where

-- Define the Dictionary class (interface) with a class constraint on keys
class (Eq k, Eq v) => Dictionary hm k v where
  empty :: Int -> hm k v -- Create An empty hash map
  insert :: k -> v -> hm k v -> hm k v -- Insert a key-value pair
  lookup :: k -> hm k v -> Maybe v -- Lookup a value by key
  delete :: k -> hm k v -> hm k v -- Delete a key-value pair
  len :: hm k v -> Int -- Return the amount of buckets
  createMap :: Int -> [(k, v)] -> hm k v -- Create map from list of pairs
  isKeyExist :: k -> hm k v -> Bool -- Check that key is inside map
  isValueExist :: k -> v -> hm k v -> Bool -- Check that value is inside map
  mapD :: ((k, v) -> (k, v)) -> hm k v -> hm k v -- Maps a function to all values in a dictionary*
  filterDK :: ((k, v) -> Bool) -> hm k v -> hm k v -- Filter over dictionary
  isEmpty :: hm k v -> Bool -- Check if HashMap buckets are empty
  getAllKeys :: hm k v -> [k] -- Return all keys from map
  getAllValues :: hm k v -> [v] -- Return all values from map
  (++) :: hm k v -> hm k v -> hm k v  -- intersects two dictionaries
