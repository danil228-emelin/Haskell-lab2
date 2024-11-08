module Main where
import HashMap
import Dictionary
main :: IO ()
main = do
    let hashmap = empty 16 :: HashMap String Int
        hashmap1 = insert "apple" 1 hashmap
        hashmap2 = insert "banana" 2 hashmap1
        hashmap3 = delete "apple" hashmap2

    print (Dictionary.lookup "banana" hashmap3)  -- Just 2
    print (Dictionary.lookup "apple" hashmap3)   -- Nothing
