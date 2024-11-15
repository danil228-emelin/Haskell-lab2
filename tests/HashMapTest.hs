module Main where
import HashMap
import Test.HUnit
import qualified System.Exit as Exit
test1 :: Test
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
