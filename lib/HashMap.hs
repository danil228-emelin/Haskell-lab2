{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HashMap (HashMap (..), Dictionary (..)) where

import Data.Array
import Data.Hashable
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Dictionary (Dictionary (..))
import Functions (customFilter, customFoldl, customFoldr, customMap)
import Test.QuickCheck

data HashMap k v
  = HashMap
      { buckets :: Array Int [(k, v)], -- Each bucket holds a list of key-value pairs
        size :: Int -- Number of buckets
      }
  | Nil
  deriving (Show, Eq)

-- Helper functions
bucketIndex :: (Hashable k) => HashMap k v -> k -> Int
bucketIndex hashmap key = hash key `mod` size hashmap

lookupHelper :: (Eq k, Eq v) => k -> [(k, v)] -> Maybe v
lookupHelper key = customFoldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

instance (Hashable k, Ord k, Ord v) => Dictionary HashMap k v where
  empty s
    | s >= 0 = HashMap (array (0, s) [(i, []) | i <- [0 .. s]]) s
    | otherwise = Nil
  insert _ _ Nil = Nil
  insert key value hashmap =
    let index = bucketIndex hashmap key
        oldBucket = buckets hashmap ! index
        newBucket = sortOn fst ((key, value) : customFilter (\(k, _) -> k /= key) oldBucket)
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
  isEmpty Nil = True
  isEmpty (HashMap buckets s) = customFoldr (\x y -> null x && y) True (elems buckets)
  getAllKeys Nil = []
  getAllKeys (HashMap buckets s) = concat [map fst x | x <- elems buckets]
  getAllValues Nil = []
  getAllValues (HashMap buckets s) = concat [map snd x | x <- elems buckets]

  (++) Nil hashMap = hashMap
  (++) hashMap Nil = hashMap
  (++) hashMap hashMap2 =
    let commonElems = [(k, min v (fromMaybe v (Dictionary.lookup k hashMap2))) | xs <- elems (buckets hashMap), (k, v) <- xs, isKeyExist k hashMap2]
     in createMap (length commonElems) commonElems

instance Arbitrary (HashMap String Int) where
  arbitrary = do
    amount <- choose (1, 10)
    return (createMap amount ([(show i, i) | i <- [0 .. amount]])) :: Gen (HashMap String Int)
