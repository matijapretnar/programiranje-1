import Test.HUnit
import IskalnoDrevo

testPoisciNeprazno = TestCase $
    assertEqual "poisci prazno" False (poisci Prazno 3)

testNajboljLevi = TestCase $
    assertEqual "najbolj levi" 1 (najboljLevi $ iskalnoDrevo [2, 7, 1, 8, 2, 1, 8])
    
testIskalnoDrevo =
    TestList [
        testPoisciNeprazno,
        testNajboljLevi
    ]

main = do
    let x = iskalnoDrevo [10, 5, 2, 8, 16, 20] :: Drevo Int
    let x' = dodaj x 10
    let x'' = izbrisi x 8
    print x''
    runTestTT testIskalnoDrevo
