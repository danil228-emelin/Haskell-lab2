{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.QuickCheck
import HashMap
import qualified System.Exit as Exit
import Data.List (sort) 

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