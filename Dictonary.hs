module Dictionary where

class Dictionary a where
  createMap :: (Ord k) => [(k, b)] -> a

  size :: a -> Int
  
  -- questions
  getValue :: (Ord k) => k -> a -> Maybe b

  addValue :: (Ord k) => (k, b) -> a -> a

  isKeyExist :: (Ord k) => k -> a -> Bool

  isValueExist :: (Ord k) => (k, b) -> a

  deleteValue :: (Ord k) => (k, b) -> a -> a

  -- intersects two dictionaries
  (++) :: a -> a -> a

  -- union two dictionaries
  (*) :: a -> a -> a

  -- Maps a function to all values in a dictionary
  mapD :: (Dictionary b) => (a -> b) -> a -> b

  -- Maps a function to all keys in a dictionary
  mapDkeys :: (Dictionary b) => (a -> b) -> a -> b

  -- Filter over dictionary values
  filterD :: (a -> Bool) -> a

  -- Filter over keys in a dictionary
  filterDkeys :: (a -> Bool) -> a

  isEmpty :: a -> Bool

  getAllKeys :: (Ord b) => a -> [b]

  getAllValues :: a -> [b]
