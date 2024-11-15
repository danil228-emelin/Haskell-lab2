# Functional programming. Assignment # 2, Data structures.

Вариант: st-dict
## Описание задания, цели и требования

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

Требования:

- Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.

- Структуры данных должны быть неизменяемыми.

- Библиотека должна быть протестирована в рамках unit testing.

- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
- Структура должна быть полиморфной.

Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
## Реализация

### Реализация кастомных функций
```haskell
module Functions where

customFoldr :: (a -> b -> b) -> b -> [a] -> b
customFoldr _ v [] = v
customFoldr f v (x : xs) = f x (customFoldr f v xs)

customFoldl :: (a -> b -> b) -> b -> [a] -> b
customFoldl _ v [] = v
customFoldl f v (x : xs) = customFoldl f (f x v) xs

customMap :: (a -> b) -> [a] -> [b]
customMap f = customFoldr (\x y -> f x : y) []

customFilter :: (a -> Bool) -> [a] -> [a]
customFilter f = customFoldr (\x y -> if f x then x : y else y) []

```
### Создание класса (интерфейса)
```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  (++) :: hm k v -> hm k v -> hm k v -- intersects two dictionaries

```
### Реализация доп функций для HashMap
```haskell
-- Helper functions
bucketIndex :: (Hashable k) => HashMap k v -> k -> Int
bucketIndex hashmap key = hash key `mod` size hashmap

lookupHelper :: (Eq k, Eq v) => k -> [(k, v)] -> Maybe v
lookupHelper key = customFoldr (\(k, v) acc -> if k == key then Just v else acc) Nothing
```

### Реализация типа. 
```haskell
data HashMap k v
  = HashMap
      { buckets :: Array Int [(k, v)], -- Each bucket holds a list of key-value pairs
        size :: Int -- Number of buckets
      }
  | Nil
  deriving (Show, Eq)

```
## Реализация интерфейс dictionary
```haskell
instance (Hashable k, Ord k, Ord v) => Dictionary HashMap k v where
```

## Реализация insert
```haskell
insert _ _ Nil = Nil
  insert key value hashmap =
    let index = bucketIndex hashmap key
        oldBucket = buckets hashmap ! index
        newBucket = sortOn fst ((key, value) : customFilter (\(k, _) -> k /= key) oldBucket)
        newBuckets = buckets hashmap // [(index, newBucket)]
     in hashmap {buckets = newBuckets}
```
## Реализация empty
```haskell
  empty s
    | s >= 0 = HashMap (array (0, s) [(i, []) | i <- [0 .. s]]) s
    | otherwise = Nil
```
## Реализация lookup
```haskell
lookup _ Nil = Nothing
  lookup key hashmap =
    let index = bucketIndex hashmap key
        bucket = buckets hashmap ! index
     in lookupHelper key bucket
```
## Реализация delete
```haskell
 delete _ Nil = Nil
  delete key hashmap =
    let index = bucketIndex hashmap key
        oldBucket = buckets hashmap ! index
        newBucket = customFilter (\(k, _) -> k /= key) oldBucket
        newBuckets = buckets hashmap // [(index, newBucket)]
     in hashmap {buckets = newBuckets}
```
## Реализация len
```haskell
  len Nil = 0
  len hashmap = size hashmap
```
## Реализация createMap
```haskell
  createMap s [] = empty s
  createMap s xs =
    let hashmap = empty s
     in customFoldl (\(k, v) acc -> insert k v acc) hashmap xs
```
## Реализация isKeyExist
```haskell
  isKeyExist key Nil = False
  isKeyExist key hashMap =
    let index = bucketIndex hashMap key
        bucket = buckets hashMap ! index
     in customFoldl (\(k, v) acc -> acc || k == key) False bucket
```
## Реализация isValueExist
```haskell
 isValueExist _ _ Nil = False
  isValueExist key value hashMap =
    let index = bucketIndex hashMap key
        bucket = buckets hashMap ! index
     in customFoldl (\(k, v) acc -> acc || value == v) False bucket
```
## Реализация mapD
```haskell
  mapD _ Nil = Nil
  mapD f (HashMap buckets s) =
    let newBuckets = [customMap f x | x <- elems buckets]
     in HashMap (array (0, s) [(i, newBuckets !! i) | i <- [0 .. s]]) s
```
## Реализация filterDK
```haskell
  filterDK _ Nil = Nil
  filterDK f (HashMap buckets s) =
    let newBuckets = [filter f x | x <- elems buckets]
     in HashMap (array (0, s) [(i, newBuckets !! i) | i <- [0 .. s]]) s
```
## Реализация isEmpty
```haskell
  isEmpty Nil = True
  isEmpty (HashMap buckets s) = customFoldr (\x y -> null x && y) True (elems buckets)
```
## Реализация getAllKeys
```haskell
  getAllKeys Nil = []
  getAllKeys (HashMap buckets s) = concat [map fst x | x <- elems buckets]
```
## Реализация getAllValues
```haskell
  getAllValues Nil = []
  getAllValues (HashMap buckets s) = concat [map snd x | x <- elems buckets]
```
## Реализация (++)
```haskell
  (++) Nil hashMap = hashMap
  (++) hashMap Nil = hashMap
  (++) hashMap hashMap2 =
    let commonElems = [(k, min v (fromMaybe v (Dictionary.lookup k hashMap2))) | xs <- elems (buckets hashMap), (k, v) <- xs, isKeyExist k hashMap2]
     in createMap (length commonElems) commonElems
```
## Реализация Arbitrary для property-based тестов
```haskell
instance Arbitrary (HashMap String Int) where
  arbitrary = do
    amount <- choose (1, 10)
    return (createMap amount ([(show i, i) | i <- [0 .. amount]])) :: Gen (HashMap String Int)
```

## Реализация Hunit тестов. Протестировано все API
```haskell
test1 = TestCase (assertEqual "check work of len func" 2 (len (createMap 2 [("A",1),("B",2),("C",3)]:: HashMap String Int)))

test2 = TestCase (assertEqual "check work of empty func" Nil (empty (-4)::HashMap String Int))

test3 = TestCase (assertEqual "check work of insert func" (createMap 2 [("A",1),("B",2)]) (insert "B" 2 (createMap 2 [("A",1)]:: HashMap String Int)))

test4 = TestCase (assertEqual "check work of insert func when it rewriting old value" (createMap 2 [("A",2)]) (insert "A" 2 (createMap 2 [("A",1)]:: HashMap String Int)))

test5 = TestCase (assertEqual "check work of lookup func when value is present" (Just 1) (HashMap.lookup "A" (createMap 2 [("A",1)]:: HashMap String Int)))

test6 = TestCase (assertEqual "check work of lookup func when value is absent" (Nothing) (HashMap.lookup "B" (createMap 2 [("A",1)]:: HashMap String Int)))

test7 = TestCase (assertEqual "check work of delete func when value is present" (createMap 2 [("A",1)]) (delete "B" (createMap 2 [("A",1),("B",2)]:: HashMap String Int)))

test8 = TestCase (assertEqual "check work of delete func when value is absent" (createMap 2 [("A",1)]) (delete "B" (createMap 2 [("A",1)]:: HashMap String Int)))

test9 = TestCase (assertEqual "check work of isKeyExist func when value is present" (True) (isKeyExist "A" (createMap 2 [("A",1)]:: HashMap String Int)))

test10= TestCase (assertEqual "check work of isKeyExist func when value is absent" (False) (isKeyExist "B" (createMap 2 [("A",1)]:: HashMap String Int)))

test11 = TestCase (assertEqual "check work of isValueExist func when value is present" (True) (isValueExist "A" 1 (createMap 2 [("A",1)]:: HashMap String Int)))

test12= TestCase (assertEqual "check work of isValueExist func when value is absent" (False) (isValueExist "B" 12 (createMap 2 [("A",1)]:: HashMap String Int)))

test13= TestCase (assertEqual "check work of mapD func" (createMap 2 [("A",1),("B",4)]) (mapD (\(x,y)->(x,y^2)) (createMap 2 [("A",1),("B",2)]:: HashMap String Int)))

test14= TestCase (assertEqual "check work of filterDK func" (createMap 2  [("A",1),("ABB",2)]) (filterDK (\(x,y)->head x == 'A') (createMap 2 [("A",1),("ABB",2),("CDF",31)]:: HashMap String Int)))

test15= TestCase (assertEqual "check work of getAllKeys func" (["A","ABB","CDF"]) (getAllKeys (createMap 2 [("A",1),("CDF",31),("ABB",2)]:: HashMap String Int)))

test16= TestCase (assertEqual "check work of getAllValues func" ([1,2,31]) (getAllValues (createMap 2 [("A",1),("CDF",31),("ABB",2)]:: HashMap String Int)))


tests :: Test
tests = TestList [TestLabel "test1" test1,TestLabel "test2" test2,TestLabel "test3" test3,
        TestLabel "test4" test4,TestLabel "test5" test5,TestLabel "test6" test6,
        TestLabel "test7" test7,TestLabel "test8" test8,TestLabel "test9" test9,
        TestLabel "test10" test10,TestLabel "test11" test11,TestLabel "test12" test12,
        TestLabel "test13" test13,TestLabel "test14" test14,TestLabel "test15" test15,
        TestLabel "test16" test16]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

```
## Реализация Property-based тестов
```haskell
-- Property: check associativity for any HashMap x y, x+y==(y+x)
prop_monoid :: HashMap String Int -> HashMap String Int-> Bool
prop_monoid a b = (a HashMap.++ b) == ( b HashMap.++ a)

-- Property: check work with neutral element for any HashMap x
prop_neutral_element :: HashMap String Int -> Bool
prop_neutral_element a = (a HashMap.++ Nil) == a

return []
runTests = $quickCheckAll
-- Run the test
main :: IO ()
main = do
  _ <- runTests
  return ()
```
## Выводы 
В ходе выполнения лабораторной работы были изучены принципы построения структур данных в Haskell. Был объявлен тип данных с двумя конструкторами и реализован базовый интерфейс для этой структуры данных. Также новым оказалось понятие Property-based testing, где структура данных тестировалась на наличие определенных свойств.Сама реализация структуры в Haskell оказалось непростой задачей.Сложно уместить понятие неизменяемости с HashMap,сложно жить с настолько строгой типизацией.Уходило очень много времени разрулить конфликты.

