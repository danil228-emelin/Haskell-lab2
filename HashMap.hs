
module HashMap  where
import Dictionary (Dictionary(..))
import Functions (customFilter, customFoldl, customFoldr, customMap)
import Data.Array
import Data.Hashable


data HashMap k v = HashMap {
    buckets :: Array Int [(k, v)],  -- Each bucket holds a list of key-value pairs
    size :: Int                     -- Number of buckets
} | Nil deriving (Show)

-- Helper functions
bucketIndex :: (Hashable k) => HashMap k v -> k -> Int
bucketIndex hashmap key = hash key `mod` size hashmap

lookupHelper :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupHelper key = customFoldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

instance (Hashable k) => Dictionary HashMap k v  where
    empty s | s >= 0 =HashMap (array (0, s) [(i, []) | i <- [0..s]]) s
            | otherwise = Nil

    insert key value hashmap =
        let index = bucketIndex hashmap key
            oldBucket = buckets hashmap ! index
            newBucket = (key, value) : customFilter (\(k, _) -> k /= key) oldBucket
            newBuckets = buckets hashmap // [(index, newBucket)]
        in hashmap { buckets = newBuckets }       

    lookup key hashmap =
        let index = bucketIndex hashmap key
            bucket = buckets hashmap ! index
        in lookupHelper key bucket     
    
    delete key hashmap =
        let index = bucketIndex hashmap key
            oldBucket = buckets hashmap ! index
            newBucket = customFilter (\(k, _) -> k /= key) oldBucket
            newBuckets = buckets hashmap // [(index, newBucket)]
        in hashmap { buckets = newBuckets }

    len hashmap = size hashmap

    createMap s [] = empty s
    createMap s xs = 
        let hashTable = empty s
        in customFoldl (\(k,v) acc -> insert k v acc) hashTable xs