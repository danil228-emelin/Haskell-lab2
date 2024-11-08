{-# LANGUAGE FlexibleInstances #-}
module Dictionary where 
-- Define the MyHashMap class with a class constraint on keys
class (Eq k) => Dictionary hm k v where
    empty :: Int-> hm k v                  -- An empty hash map
    insert :: k -> v -> hm k v  -> hm k v   -- Insert a key-value pair
    lookup :: k -> hm k v  -> Maybe v -- Lookup a value by key
    delete :: k -> hm k v  -> hm k v   -- Delete a key-value pair