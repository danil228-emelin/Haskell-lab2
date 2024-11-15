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
