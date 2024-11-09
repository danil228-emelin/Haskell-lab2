module HashMap where

import Data.Array
import Data.Hashable
import Data.List (find)
import Dictionary (Dictionary (..))
import Functions (customFilter, customFoldl, customFoldr, customMap)

data HashMap k v
  = HashMap
      { buckets :: Array Int [(k, v)], -- Each bucket holds a list of key-value pairs
        size :: Int -- Number of buckets
      }
  | Nil
  deriving (Show)

-- Helper functions
bucketIndex :: (Hashable k) => HashMap k v -> k -> Int
bucketIndex hashmap key = hash key `mod` size hashmap

lookupHelper :: (Eq k, Eq v) => k -> [(k, v)] -> Maybe v
lookupHelper key = customFoldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

instance (Hashable k, Eq v) => Dictionary HashMap k v where
  empty s
    | s >= 0 = HashMap (array (0, s) [(i, []) | i <- [0 .. s]]) s
    | otherwise = Nil
  insert _ _ Nil = Nil
  insert key value hashmap =
    let index = bucketIndex hashmap key
        oldBucket = buckets hashmap ! index
        newBucket = (key, value) : customFilter (\(k, _) -> k /= key) oldBucket
        newBuckets = buckets hashmap // [(index, newBucket)]
     in hashmap {buckets = newBuckets}
  lookup _ Nil = Nothing
  lookup key hashmap =
    let index = bucketIndex hashmap key
        bucket = buckets hashmap ! index
     in lookupHelper key bucket
  delete _ Nil = Nil
  delete key hashmap =
    let index = bucketIndex hashmap key
        oldBucket = buckets hashmap ! index
        newBucket = customFilter (\(k, _) -> k /= key) oldBucket
        newBuckets = buckets hashmap // [(index, newBucket)]
     in hashmap {buckets = newBuckets}

  len Nil = 0
  len hashmap = size hashmap

  createMap s [] = empty s
  createMap s xs =
    let hashmap = empty s
     in customFoldl (\(k, v) acc -> insert k v acc) hashmap xs

  isKeyExist key Nil = False
  isKeyExist key hashMap =
    let index = bucketIndex hashMap key
        bucket = buckets hashMap ! index
     in customFoldl (\(k, v) acc -> acc || k == key) False bucket

  isValueExist _ _ Nil = False
  isValueExist key value hashMap =
    let index = bucketIndex hashMap key
        bucket = buckets hashMap ! index
     in customFoldl (\(k, v) acc -> acc || value == v) False bucket

  mapD _ Nil = Nil
  mapD f (HashMap buckets s) =
    let newBuckets = [customMap f x | x <- elems buckets]
     in HashMap (array (0, s) [(i, newBuckets !! i) | i <- [0 .. s]]) s

  filterDK _ Nil = Nil
  filterDK f (HashMap buckets s) =
    let newBuckets = [filter f x | x <- elems buckets]
     in HashMap (array (0, s) [(i, newBuckets !! i) | i <- [0 .. s]]) s
